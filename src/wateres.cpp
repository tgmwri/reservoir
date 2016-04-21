#include <Rcpp.h>
#include "wateres.h"

using namespace std;
using namespace Rcpp;

/**
  * - converts one value from m3.s-1 to m3 per time step or other way round
  * @param value value to be converted
  * @param minutes numbers of minutes in the corresponding time step
  * @param to_volume whether to convert to m3 per time step
  * @return converted value
  */
double convert_m3(double value, unsigned minutes, bool to_volume)
{
  unsigned coeff = 60 * minutes;
  if (to_volume)
    return value * coeff;
  else
    return value / coeff;
}

/**
  * - converts vector of values from m3.s-1 to m3 per time step or other way round
  * @param values values to be converted
  * @param minutes numbers of minutes in the corresponding time steps
  * @param to_volume whether to convert to m3 per time step
  * @return vector of converted values
  */
void convert_m3(vector<double> &values, const vector<unsigned> &minutes, bool to_volume)
{
  unsigned val_count = values.size();
  for (unsigned val = 0; val < val_count; val++) {
    values[val] = convert_m3(values[val], minutes[val], to_volume);
  }
}

/**
  * - converts m3.s-1 to m3 per time step or other way round
  * @param Rvalues time series of values
  * @param Rminutes numbers of minutes in time steps of time series, must be an integer
  * @param Rto_volume whether to convert to m3 per time step
  * @return vector of converted values
  */
RcppExport SEXP convert_m3(SEXP Rvalues, SEXP Rminutes, SEXP Rto_volume)
{
  vector<double> values = as<vector<double> >(Rvalues);
  vector<unsigned> minutes = as<vector<unsigned> >(Rminutes);
  bool to_volume = as<bool>(Rto_volume);

  convert_m3(values, minutes, to_volume);
  return wrap(values);
}

//converts mm to m3
//value in mm, area in m2
double convert_mm(double value, double area)
{
  return value / 1e3 * area;
}

//!variable names: inflow, evaporation, withdrawal, precipitation, yield, deficit (not input variable), transfer
const string wateres::var_names[wateres::var_count] = {"Q", "E", "W", "P", "Y", "D", "T"};

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
  this->minutes = as<vector<unsigned> >(reser["minutes"]);
  area = as<double>(reser.attr("area"));
  eas = as<DataFrame>(reser.attr("eas"));
  transfer_add = true;
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
 * - sets values of variables in a time step to zero
 * - the following sequence applied: yield -> withdrawal -> transfer (if negative)
 * @param ts time step to be calculated
 * @param var_n identification of the variable to start with
 */
void wateres::set_var_zero(unsigned ts, var_name var_n)
{
  if (!(var_n == TRANSFER && var[var_n][ts] > 0))
    var[var_n][ts] = 0;
  switch (var_n) {
    case YIELD:
      set_var_zero(ts, WITHDRAWAL);
      break;
    case WITHDRAWAL:
      set_var_zero(ts, TRANSFER);
    default:
      break;
  }
}

/**
 * - calculates reservoir water balance for a time step starting from given variable
 * - the following sequence applied: transfer added -> evaporation -> yield -> withdrawal -> transfer removed
 * @param ts time step to be calculated
 * @param var_n identification of the variable to start with
 */
void wateres::calc_balance_var(unsigned ts, var_name var_n)
{
  //only positive transfer is added (firstly) and negative transfer is removed (lastly)
  if (var_n == TRANSFER) {
    if (transfer_add && var[var_n][ts] < 0) {
      transfer_add = false;
      calc_balance_var(ts, PRECIPITATION);
      return;
    }
    else if (!transfer_add && var[var_n][ts] > 0) {
      return;
    }
  }
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
    case TRANSFER:
      tmp_coeff = 1;
      break;
    default:
      break;
  }
  storage[ts + 1] += var[var_n][ts] * tmp_coeff;
  if (storage[ts + 1] < 0) {
    if (var_n == TRANSFER)
      var[var_n][ts] -= storage[ts + 1];
    else
      var[var_n][ts] += storage[ts + 1];
    storage[ts + 1] = 0;
    switch (var_n) {
      case EVAPORATION:
        set_var_zero(ts, YIELD);
        break;
      case YIELD:
        set_var_zero(ts, WITHDRAWAL);
        break;
      case WITHDRAWAL:
        set_var_zero(ts, TRANSFER);
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
        calc_balance_var(ts, TRANSFER);
        break;
      case TRANSFER:
        if (transfer_add) {
          transfer_add = false;
          calc_balance_var(ts, PRECIPITATION);
        }
        break;
      default:
        break;
    }
    if (storage[ts + 1] > volume) {
      if (!throw_exceed)
        var[YIELD][ts] += storage[ts + 1] - volume;
      storage[ts + 1] = volume;
    }
  }
}

/**
  * - calculates monthly time series of reservoir storage and yield
  * @param Rreser reservoir object with time series of inflows (Q) in m3.s-1, precipitation (R) in mm, evaporation (E) in mm,
    withdrawal in m3, number of minutes in time steps (minutes) and with attributes (area - flooded by reservoir in m2,
    eas - elevation-area-storage relationship (in m.a.s.l., m2 and m3)
  * @param Rinflow time series of inflows in m3.s-1
  * @param Ryield_req time series of required yield (reservoir outflow) in m3.s-1
  * @param Rvolume reservoir potential volume in m3
  * @param Rinitial_storage initial storage in the reservoir in m3
  * @param Rthrow_exceed whether volume exceeding maximum storage will be thrown or added to yield
  * @return list consisting of storage (in m3), yield (m3.s-1), precipitation (m3), evaporation (m3) and withdrawal (m3)
  */
RcppExport SEXP calc_storage(SEXP Rreser, SEXP Ryield_req, SEXP Rvolume, SEXP Rinitial_storage, SEXP Rthrow_exceed)
{
  DataFrame reser = as<DataFrame>(Rreser);
  vector<double> yield_req = as<vector<double> >(Ryield_req);
  double volume = as<double>(Rvolume);
  double initial_storage = as<double>(Rinitial_storage);
  bool throw_exceed = as<bool>(Rthrow_exceed);

  unsigned ts;
  //reser.nrows() incorrect when subset of data.table used and its attributes are copied afterwards
  //-> take length of arbitrary column which is not affected by the attributes
  unsigned time_steps = as<NumericVector>(reser["minutes"]).size();

  vector<double> storage(time_steps + 1, 0);
  storage[0] = initial_storage;

  wateres reservoir(reser, storage, throw_exceed, volume);
  convert_m3(yield_req, reservoir.minutes, true);
  convert_m3(reservoir.var[wateres::INFLOW], reservoir.minutes, true);
  bool is_transfer = false;
  for (ts = 0; ts < time_steps; ts++) {
    reservoir.var[wateres::YIELD][ts] = yield_req[ts];
    reservoir.storage[ts + 1] = reservoir.storage[ts] + reservoir.var[wateres::INFLOW][ts];
    double withdrawal_req = reservoir.var[wateres::WITHDRAWAL][ts];
    reservoir.transfer_add = true;
    reservoir.calc_balance_var(ts, wateres::TRANSFER);
    double diff_yield = yield_req[ts] - reservoir.var[wateres::YIELD][ts];
    if (diff_yield > 0)
      reservoir.var[wateres::DEFICIT][ts] += diff_yield;
    double diff_withdrawal = withdrawal_req - reservoir.var[wateres::WITHDRAWAL][ts];
    if (diff_withdrawal > 0)
      reservoir.var[wateres::DEFICIT][ts] += diff_withdrawal;
    if (abs(reservoir.var[wateres::TRANSFER][ts]) > numeric_limits<double>::epsilon())
      is_transfer = true;
  }
  List resul;
  resul["storage"] = reservoir.storage;
  convert_m3(reservoir.var[wateres::YIELD], reservoir.minutes, false);
  resul["yield"] = reservoir.var[wateres::YIELD];
  resul["precipitation"] = reservoir.var[wateres::PRECIPITATION];
  resul["evaporation"] = reservoir.var[wateres::EVAPORATION];
  resul["withdrawal"] = reservoir.var[wateres::WITHDRAWAL];
  resul["deficit"] = reservoir.var[wateres::DEFICIT];
  if (is_transfer)
    resul["transfer"] = reservoir.var[wateres::TRANSFER];

  return resul;
}
