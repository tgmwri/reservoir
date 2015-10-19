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

//converts mm to m3
//value in mm, area in m2
double convert_mm(double value, double area)
{
  return value / 1e3 * area;
}

//!variable names (inflow, evaporation, withdrawal, precipitation, yield)
const string wateres::var_names[wateres::var_count] = {"Q", "E", "W", "P", "Y"};

/**
 * - creates water reservoir from given vectors of variables and options
 */
wateres::wateres(
  DataFrame reser, vector<double> storage, bool throw_exceed, double volume) : storage(storage),
  throw_exceed(throw_exceed), volume(volume)
{
  unsigned row_count = reser.nrows();
  vector<string> col_names = as<vector<string> >(reser.attr("names"));
  var.resize(var_count);
  for (unsigned v = 0; v < var_count; v++) {
    if (find(col_names.begin(), col_names.end(), var_names[v]) != col_names.end()) {
      var[v] = as<vector<double> >(reser[var_names[v]]);
    }
    else {
      var[v].resize(row_count, 0);
    }
  }
  this->days = as<vector<unsigned> >(reser[".days"]);
  area = as<double>(reser.attr("area"));
  eas = as<DataFrame>(reser.attr("eas"));
}

/**
 * - makes linear interpolation to get flooded area for given storage
 * - if storage out of given limits, a limit value is returned
 * @param storage_req reservoir storage in m3
 * @return area in m2
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
  return 0;
}

/**
 * - calculates reservoir water balance for a time step starting from given variable
 * - sequence evaporation -> yield -> withdrawal applied
 * @param ts time step to be calculated
 * @param var_n identification of the variable to start with
 */
void wateres::calc_balance_var(unsigned ts, var_name var_n)
{
  int tmp_coeff = -1; //whether to add or subtract in the balance
  switch (var_n) {
    case PRECIPITATION:
      tmp_coeff = 1;
      var[var_n][ts] = convert_mm(var[var_n][ts], area);
      break;
    case EVAPORATION:
      double tmp_area;
      if (eas.size() == 0)
        tmp_area = area;
      else
        tmp_area = get_area(storage[ts]);
      var[var_n][ts] = convert_mm(var[var_n][ts], tmp_area);
      break;
    default:
      break;
  }
  storage[ts + 1] += var[var_n][ts] * tmp_coeff;
  if (storage[ts + 1] < 0) {
    var[var_n][ts] = var[var_n][ts] + storage[ts + 1];
    storage[ts + 1] = 0;
    switch (var_n) {
      case EVAPORATION:
        var[YIELD][ts] = 0;
        var[WITHDRAWAL][ts] = 0;
        break;
      case YIELD:
        var[WITHDRAWAL][ts] = 0;
        break;
      default:
        break;
    }
  }
  else {
    switch (var_n) {
      case PRECIPITATION:
        calc_balance_var(ts, EVAPORATION);
        break;
      case EVAPORATION:
        calc_balance_var(ts, YIELD);
        break;
      case YIELD:
        calc_balance_var(ts, WITHDRAWAL);
        break;
      case WITHDRAWAL:
        if (storage[ts + 1] > volume) {
          if (!throw_exceed)
            var[YIELD][ts] += storage[ts + 1] - volume;
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
  * @param Rreser reservoir object with time series of inflows (Q) in m3.s-1, precipitation (R) in mm, evaporation (E) in mm,
    withdrawal in m3, number of days in months (.days) and with attributes (area - flooded by reservoir in m2, eas - elevation-area-storage
    relationship (in m.a.s.l., m2 and m3)
  * @param Rinflow time series of inflows in m3.s-1
  * @param Ryield_req required yield (reservoir outflow) in m3.s-1
  * @param Rvolume reservoir potential volume in m3
  * @param Rinitial_storage initial storage in the reservoir in m3
  * @param Rthrow_exceed whether volume exceeding maximum storage will be thrown or added to yield
  * @return list consisting of storage (in m3), yield (m3.s-1), precipitation (m3), evaporation (m3) and withdrawal (m3)
  */
RcppExport SEXP calc_storage(SEXP Rreser, SEXP Ryield_req, SEXP Rvolume, SEXP Rinitial_storage, SEXP Rthrow_exceed)
{
  DataFrame reser = as<DataFrame>(Rreser);
  double yield_req = as<double>(Ryield_req);
  double volume = as<double>(Rvolume);
  double initial_storage = as<double>(Rinitial_storage);
  bool throw_exceed = as<bool>(Rthrow_exceed);

  unsigned ts;
  //reser.nrows() incorrect when subset of data.table used and its attributes are copied afterwards
  //-> take length of arbitrary column which is not affected by the attributes
  unsigned time_steps = as<NumericVector>(reser[".days"]).size();

  vector<double> storage(time_steps + 1, 0);
  storage[0] = initial_storage;

  wateres reservoir(reser, storage, throw_exceed, volume);
  vector<double> yield_req_vol(time_steps, yield_req);
  convert_m3(yield_req_vol, reservoir.days, true);
  convert_m3(reservoir.var[wateres::INFLOW], reservoir.days, true);
  for (ts = 0; ts < time_steps; ts++) {
    reservoir.var[wateres::YIELD][ts] = yield_req_vol[ts];
    reservoir.storage[ts + 1] = reservoir.storage[ts] + reservoir.var[wateres::INFLOW][ts];
    reservoir.calc_balance_var(ts, wateres::PRECIPITATION);
  }
  List resul;
  resul["storage"] = reservoir.storage;
  convert_m3(reservoir.var[wateres::YIELD], reservoir.days, false);
  resul["yield"] = reservoir.var[wateres::YIELD];
  resul["precipitation"] = reservoir.var[wateres::PRECIPITATION];
  resul["evaporation"] = reservoir.var[wateres::EVAPORATION];
  resul["withdrawal"] = reservoir.var[wateres::WITHDRAWAL];

  return resul;
}
