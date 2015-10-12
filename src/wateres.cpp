#include <Rcpp.h>
#include "wateres.h"

using namespace std;
using namespace Rcpp;

// converts between m3.s-1 and m3 per month
void convert_m3(vector<double> &values, const vector<unsigned> &days, bool to_volume)
{
  unsigned val_count = values.size();
  for (unsigned val = 0; val < val_count; val++) {
    unsigned coeff = 3600 * 24 * days[val];
    if (to_volume)
      values[val] *= coeff;
    else
      values[val] /= coeff;
  }
}

/**
  * - converts m3.s-1 to m3 per month or other way round
  * @param Rvalues time series of values
  * @param Rdays number of days for months of time series, must be an integer
  * @param Rto_volume whether to convert to m3 per month
  * @return vector of converted values
  */
RcppExport SEXP convert_m3(SEXP Rvalues, SEXP Rdays, SEXP Rto_volume)
{
  vector<double> values = as<vector<double> >(Rvalues);
  vector<unsigned> days = as<vector<unsigned> >(Rdays);
  bool to_volume = as<bool>(Rto_volume);

  convert_m3(values, days, to_volume);
  return wrap(values);
}

//value in mm, area in km2
double convert_mm_to_m3s1(double value, unsigned days, double area)
{
  value *= 1e3 * area;
  vector<double> tmp_value(1, value);
  vector<unsigned> tmp_days(1, days);
  convert_m3(tmp_value, tmp_days, false);
  return tmp_value[0];
}

/**
 * - creates water reservoir from given vectors of variables and options
 */
wateres::wateres(
  vector<double> evaporation, vector<double> withdrawal, vector<double> yield, vector<double> storage,
  vector<unsigned> days, DataFrame eas, bool throw_exceed, double volume, double area)
{
  this->evaporation = evaporation;
  this->withdrawal = withdrawal;
  this->yield = yield;
  this->storage = storage;
  this->days = days;
  this->eas = eas;
  this->throw_exceed = throw_exceed;
  this->volume = volume;
  this->area = area;
}

/**
 * - makes linear interpolation to get flooded area for given storage
 * - if storage out of given limits, a limit value is returned
 * @param storage reservoir storage in mil. m3
 * @return area in km2
 */
double wateres::get_area(double storage_req)
{
  NumericVector eas_area = eas["area"], eas_storage = eas["storage"];
  unsigned eas_size = eas_area.size();
  if (storage_req < eas_storage[0])
    return eas_area[0];
  if (storage_req > eas_storage[eas_size - 1])
    return eas_area[eas_size - 1];
  for (unsigned as = 1; as < eas_size; as++) {
    if (storage_req < eas_storage[as] + eas_storage[as] * numeric_limits<double>::epsilon()) {
      return (eas_area[as] - eas_area[as - 1]) / (eas_storage[as] - eas_storage[as - 1]) * (storage_req - eas_storage[as - 1]) + eas_area[as - 1];
    }
  }
}

/**
 * - calculates reservoir water balance for a time step starting from given variable
 * - sequence evaporation -> yield -> withdrawal applied
 * @param variable variable to start with
 * @param ts time step to be calculated
 * @param var variable identification
 */
void wateres::calc_balance_var(vector<double> &variable, unsigned ts, var_name var)
{
  switch (var) {
    case EVAPORATION:
      double tmp_area;
      if (eas.size() == 0)
        tmp_area = area;
      else
        tmp_area = get_area(storage[ts] / 1e6);
      variable[ts] = convert_mm_to_m3s1(variable[ts], days[ts], tmp_area);
      break;
    default:
      break;
  }
  storage[ts + 1] -= variable[ts];
  if (storage[ts + 1] < 0) {
    variable[ts] = variable[ts] + storage[ts + 1];
    storage[ts + 1] = 0;
    switch (var) {
      case EVAPORATION:
        yield[ts] = 0;
        withdrawal[ts] = 0;
        break;
      case YIELD:
        withdrawal[ts] = 0;
        break;
      default:
        break;
    }
  }
  else {
    switch (var) {
      case EVAPORATION:
        calc_balance_var(yield, ts, YIELD);
        break;
      case YIELD:
        calc_balance_var(withdrawal, ts, WITHDRAWAL);
        break;
      case WITHDRAWAL:
        if (storage[ts + 1] > volume) {
          if (!throw_exceed)
            yield[ts] += storage[ts + 1] - volume;
          storage[ts + 1] = volume;
        }
        break;
      default:
        break;
    }
  }
}

/**
  * - calculates monthly time series of reservoir storage and yield
  * @param Rinflow time series of inflows in m3.s-1
  * @param Rdays number of days for months of time series
  * @param Revaporation time series of evaporation in mm
  * @param Rwithdrawal time series of withdrawal in m3
  * @param Ryield_req required yield (reservoir outflow) in m3.s-1
  * @param Rvolume reservoir potential volume in millions of m3
  * @param Rinitial_storage initial storage in the reservoir in millions of m3
  * @param Rarea area flooded by reservoir in km2
  * @param Reas elevation-area-storage relationship (in m.a.s.l., km2 and mil. m3)
  * @param Rthrow_exceed whether volume exceeding maximum storage will be thrown or added to yield
  * @return list consisting of storage (in m3), yield (m3.s-1), evaporation (m3) and withdrawal (m3)
  */
RcppExport SEXP calc_storage(
  SEXP Rinflow, SEXP Rdays, SEXP Revaporation, SEXP Rwithdrawal, SEXP Ryield_req, SEXP Rvolume, SEXP Rinitial_storage,
  SEXP Rarea, SEXP Reas, SEXP Rthrow_exceed)
{
  vector<double> inflow = as<vector<double> >(Rinflow);
  vector<unsigned> days = as<vector<unsigned> >(Rdays);
  vector<double> evaporation = as<vector<double> >(Revaporation);
  vector<double> withdrawal = as<vector<double> >(Rwithdrawal);
  double yield_req = as<double>(Ryield_req);
  DataFrame eas = as<DataFrame>(Reas);
  double volume = as<double>(Rvolume) * 1e6;
  double initial_storage = as<double>(Rinitial_storage) * 1e6;
  double area = as<double>(Rarea);
  bool throw_exceed = as<bool>(Rthrow_exceed);

  unsigned time_steps = inflow.size(), ts;
  convert_m3(inflow, days, true);
  convert_m3(evaporation, days, true);
  vector<double> yield_req_vol(time_steps, yield_req);
  convert_m3(yield_req_vol, days, true);

  vector<double> yield(time_steps, 0), storage(time_steps + 1, 0);
  storage[0] = initial_storage;

  wateres reser(evaporation, withdrawal, yield, storage, days, eas, throw_exceed, volume, area);
  for (ts = 0; ts < time_steps; ts++) {
    reser.yield[ts] = yield_req_vol[ts];
    reser.storage[ts + 1] = reser.storage[ts] + inflow[ts];
    reser.calc_balance_var(reser.evaporation, ts, wateres::EVAPORATION);
  }
  List resul;
  resul["storage"] = reser.storage;
  convert_m3(reser.yield, days, false);
  resul["yield"] = reser.yield;
  resul["evaporation"] = reser.evaporation;
  resul["withdrawal"] = reser.withdrawal;

  return resul;
}
