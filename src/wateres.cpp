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

//!variable names: inflow, evaporation, water use, precipitation, yield, deficit (not input variable), transfer
const string wateres::var_names[wateres::var_count] = {"Q", "E", "W", "P", "Y", "D", "T", "YU"};

/**
 * - creates water reservoir from given vectors of variables and options
 */
wateres::wateres(
  DataFrame reser, vector<double> storage, bool throw_exceed, vector<double> volume, vector<double> volume_optim,
    vector<double> yield_max) : storage(storage), throw_exceed(throw_exceed), volume(volume), volume_optim(volume_optim),
    yield_max(yield_max)
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
  //custom inflow from another reservoir instead of natural inflow
  if (find(col_names.begin(), col_names.end(), "I") != col_names.end()) {
    var[INFLOW] = as<vector<double> >(reser["I"]);
  }

  this->minutes = as<vector<unsigned> >(reser["minutes"]);
  area = as<double>(reser.attr("area"));

  double tmp[5] = { 0, 0.1, 0.3, 0.5, 0.75 };
  plant_covers.assign(&tmp[0], &tmp[0] + 5);
  double tmp2[5] = { 1, 1.03, 1.08, 1.14, 1.22 };
  plant_coeffs.assign(&tmp2[0], &tmp2[0] + 5);

  Rcpp::Nullable<double> tmp_plant = as<Rcpp::Nullable<double> >(reser.attr("plant_cover"));
  if (tmp_plant.isNotNull())
    plant_cover = as<double>(tmp_plant);
  else
    plant_cover = 0;
  plant_coeff = interpolate_linear(plant_covers, plant_coeffs, plant_cover);
  eas = as<DataFrame>(reser.attr("eas"));
  transfer_add = true;
}

/**
 * - makes linear interpolation
 * - if given value is out of given limits, a limit value is returned
 * @param x X values
 * @param y Y values
 * @param x_required X value for which Y value is required
 * @return Y value
 */
double wateres::interpolate_linear(vector<double> &x, vector<double> &y, double x_required)
{
  unsigned x_size = x.size();
  if (x_size > 0 && x_size == y.size()) {
    if (x_required < x[0])
      return y[0];
    if (x_required > x[x_size - 1])
      return y[x_size - 1];
    for (unsigned n = 1; n < x_size; n++) {
      if (x_required < x[n] + x[n] * numeric_limits<double>::epsilon()) {
        return (y[n] - y[n - 1]) / (x[n] - x[n - 1]) * (x_required - x[n - 1]) + y[n - 1];
      }
    }
  }
  return 0;
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
  vector<double> tmp_storage = as<vector<double> >(eas_storage);
  vector<double> tmp_area = as<vector<double> >(eas_area);
  return interpolate_linear(tmp_storage, tmp_area, storage_req);
}

/**
 * - sets values of variables in a time step to zero
 * - the following sequence applied: yield -> water use (if negative) -> transfer (if negative)
 * @param ts time step to be calculated
 * @param var_n identification of the variable to start with
 */
void wateres::set_var_zero(unsigned ts, var_name var_n)
{
  if (!((var_n == WATERUSE || var_n == TRANSFER) && var[var_n][ts] > 0))
    var[var_n][ts] = 0;
  switch (var_n) {
    case YIELD:
      set_var_zero(ts, WATERUSE);
      break;
    case WATERUSE:
      set_var_zero(ts, TRANSFER);
    default:
      break;
  }
}

/**
 * - checks if variable value has corresponding sign for adding/subtracting
 * - if it is the case, nothing happens
 * - otherwise water balance (other variables) is calculated for the time step
 * @param ts time step
 * @param var_n identification of the variable
 * @param next_var_n identification of the next variable in the calculated sequence
 * @param adding whether to consider adding or subtracting (will be changed eventually)
 * @return true if values are effective and nothing has happened
 */
bool wateres::check_value_sign(unsigned ts, var_name var_n, var_name next_var_n, bool &adding)
{
  if (adding && var[var_n][ts] < 0) {
    adding = false;
    calc_balance_var(ts, next_var_n);
    return false;
  }
  else if (!adding && var[var_n][ts] > 0) {
    if (var_n != TRANSFER) //negative transfer the last in the sequence
      calc_balance_var(ts, next_var_n);
    return false;
  }
  return true;
}

/**
 * - calculates reservoir water balance for a time step starting from given variable
 * - the following sequence applied: water use added -> transfer added -> evaporation -> yield -> water use removed -> transfer removed
 * @param ts time step to be calculated
 * @param var_n identification of the variable to start with
 */
void wateres::calc_balance_var(unsigned ts, var_name var_n)
{
  //only positive water use/transfer is added (firstly) and negative water use/transfer is removed (lastly)
  if (var_n == WATERUSE || var_n == TRANSFER) {
    if (!check_value_sign(ts, var_n, var_n == WATERUSE ? TRANSFER : PRECIPITATION, var_n == WATERUSE ? wateruse_add : transfer_add))
      return;
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
      if (plant_coeff > 1 + numeric_limits<double>::epsilon()) {
        double tmp_plant_coeff;
        if (eas.size() == 0)
          tmp_plant_coeff = plant_coeff;
        else {
          //assumed that area with plants is the shallowest
          double plant_coeff_area = max(plant_cover - 1 + tmp_area / area, 0.0);
          tmp_plant_coeff = interpolate_linear(plant_covers, plant_coeffs, plant_coeff_area);
        }
        var[var_n][ts] *= tmp_plant_coeff;
      }
      break;
    case WATERUSE:
    case TRANSFER:
      tmp_coeff = 1;
      break;
    default:
      break;
  }
  storage[ts + 1] += var[var_n][ts] * tmp_coeff;
  if (storage[ts + 1] < 0) {
    if (var_n == WATERUSE || var_n == TRANSFER)
      var[var_n][ts] -= storage[ts + 1];
    else
      var[var_n][ts] += storage[ts + 1];
    storage[ts + 1] = 0;
    switch (var_n) {
      case EVAPORATION:
        set_var_zero(ts, YIELD);
        break;
      case YIELD:
        set_var_zero(ts, WATERUSE);
        break;
      case WATERUSE:
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
        calc_balance_var(ts, WATERUSE);
        break;
      case WATERUSE:
        wateruse_add = false;
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
    // water exceeding optimum volume
    if (!volume_optim.empty() && storage[ts + 1] > volume_optim[ts + 1]) {
      if (var[YIELD][ts] < yield_max[ts]) {
        double orig_yield = var[YIELD][ts];
        var[YIELD][ts] += storage[ts + 1] - volume_optim[ts + 1];
        if (var[YIELD][ts] > yield_max[ts]) {
          var[YIELD][ts] = yield_max[ts];
        }
        storage[ts + 1] -= var[YIELD][ts] - orig_yield;
      }
    }
    // water exceeding maximum volume
    if (storage[ts + 1] > volume[ts + 1]) {
      if (!throw_exceed)
        var[YIELD][ts] += storage[ts + 1] - volume[ts + 1];
      storage[ts + 1] = volume[ts + 1];
    }
  }
}

/**
 * - calculates yield routing by lagging, modifies variable YIELD
 * @param lag_time lag time in minutes
 * @param initial_pos initial time step
 * @param time_steps number of time steps
 */
void wateres::calc_routing_lag(double lag_time, unsigned initial_pos, unsigned time_steps)
{
  unsigned ts;
  std::vector<unsigned> minutes_cumulative(minutes);
  for (ts = initial_pos; ts < time_steps; ts++) {
      if (ts > initial_pos) {
        minutes_cumulative[ts] = minutes_cumulative[ts - 1] + minutes[ts];
      }
  }

  for (ts = initial_pos; ts < time_steps; ts++) {
    unsigned lagged_begin_time = (ts > 0 ? minutes_cumulative[ts - 1] : 0) + static_cast<unsigned>(lag_time);
    unsigned lagged_end_time = lagged_begin_time + minutes[ts];
    for (unsigned next_ts = ts; next_ts < time_steps; next_ts++) {
      unsigned begin_time = next_ts > 0 ? minutes_cumulative[next_ts - 1] : 0;
      unsigned end_time = minutes_cumulative[next_ts];
      if (lagged_begin_time < end_time && lagged_end_time > begin_time) {
        unsigned minutes_for_lagged = (lagged_end_time < end_time ? lagged_end_time : end_time)
          - (lagged_begin_time > begin_time ? lagged_begin_time : begin_time);
        var[wateres::YIELD][next_ts] += var[wateres::YIELD_UNROUTED][ts] * minutes_for_lagged / minutes[ts];
      }
    }
  }
}

/**
 * - calculates yield routing by transformation in linear reservoir, modifies variable YIELD
 * @param storage_coeff storage coefficient in minutes
 * @param initial_pos initial time step
 * @param time_steps number of time steps
 */
void wateres::calc_routing_linear_reservoir(double storage_coeff, unsigned initial_pos, unsigned time_steps)
{
  double current_storage = 0;
  for (unsigned ts = initial_pos; ts < time_steps; ts++) {
    var[wateres::YIELD][ts] = current_storage / storage_coeff * minutes[ts];
    current_storage += var[wateres::YIELD_UNROUTED][ts] - var[wateres::YIELD][ts];
    if (current_storage < 0) {
      current_storage = 0;
    }
  }
}

/**
  * - calculates monthly time series of reservoir storage and yield
  * @param Rreser reservoir object with time series of inflows (Q) in m3.s-1, precipitation (R) in mm, evaporation (E) in mm,
    water use in m3, number of minutes in time steps (minutes) and with attributes: area - flooded by reservoir in m2,
    eas - elevation-area-storage relationship (in m.a.s.l., m2 and m3), plant_cover - fraction of fully flooded area covered by plants
  * @param Rinflow time series of inflows in m3.s-1
  * @param Ryield_req time series of required yield (reservoir outflow) in m3.s-1
  * @param Ryield_max time series of maximum yield (gained from storage between optimum and maximum value) in m3.s-1
  * @param Rvolume time series of reservoir potential volume in m3
  * @param Rvolume_optim time series of reservoir optimal volume in m3
  * @param Rinitial_storage initial storage in the reservoir in m3
  * @param Rinitial_pos initial time step of calculation
  * @param Rlast_pos last time step of calculation
  * @param Rthrow_exceed whether volume exceeding maximum storage will be thrown or added to yield
  * @param Rtill_deficit whether the calculation will end in the first time step with deficit
  * @param Rfirst_deficit_pos if enabled Rtill_deficit, also time step needs to be at least this one to stop the calculation
  * @return list consisting of storage (in m3), yield (m3.s-1), precipitation (m3), evaporation (m3) and water use (m3)
  */
RcppExport SEXP calc_storage(
  SEXP Rreser, SEXP Ryield_req, SEXP Ryield_max, SEXP Rvolume, SEXP Rvolume_optim, SEXP Rinitial_storage, SEXP Rinitial_pos, SEXP Rlast_pos,
  SEXP Rthrow_exceed, SEXP Rtill_deficit, SEXP Rfirst_deficit_pos, SEXP Rrouting_method, SEXP Rrouting_settings)
{
  DataFrame reser = as<DataFrame>(Rreser);
  vector<double> yield_req = as<vector<double> >(Ryield_req);
  vector<double> yield_max;
  if (!Rf_isNull(Ryield_max)) {
    yield_max = as<vector<double> >(Ryield_max);
  }
  vector<double> volume = as<vector<double> >(Rvolume);
  vector<double> volume_optim;
  if (!Rf_isNull(Rvolume_optim)) {
    volume_optim = as<vector<double> >(Rvolume_optim);
  }
  double initial_storage = as<double>(Rinitial_storage);
  unsigned initial_pos = as<unsigned>(Rinitial_pos) - 1; //from R to C++ indexing
  unsigned last_pos = as<unsigned>(Rlast_pos) - 1;
  bool throw_exceed = as<bool>(Rthrow_exceed);
  bool till_deficit = as<bool>(Rtill_deficit);
  unsigned first_deficit_pos = as<unsigned>(Rfirst_deficit_pos) - 1;

  string routing_method = as<string>(Rrouting_method);
  bool is_routing = routing_method != "none";
  List routing_settings = as<List>(Rrouting_settings);

  unsigned ts;
  //reser.nrows() incorrect when subset of data.table used and its attributes are copied afterwards
  //-> take length of arbitrary column which is not affected by the attributes
  unsigned time_steps = as<NumericVector>(reser["minutes"]).size();

  vector<double> storage(time_steps + 1, 0);
  storage[initial_pos] = initial_storage;

  convert_m3(yield_max, as<vector<unsigned> >(reser["minutes"]), true);
  wateres reservoir(reser, storage, throw_exceed, volume, volume_optim, yield_max);
  convert_m3(yield_req, reservoir.minutes, true);
  convert_m3(reservoir.var[wateres::INFLOW], reservoir.minutes, true);
  bool is_transfer = false;
  for (ts = initial_pos; ts < time_steps; ts++) {
    reservoir.var[wateres::YIELD][ts] = yield_req[ts];
    reservoir.storage[ts + 1] = reservoir.storage[ts] + reservoir.var[wateres::INFLOW][ts];
    //negative inflow should not be allowed, but just to be sure - otherwise balance calculation would be destroyed
    if (reservoir.storage[ts + 1] < 0)
      reservoir.storage[ts + 1] = 0;
    double withdrawal_req = reservoir.var[wateres::WATERUSE][ts];
    reservoir.transfer_add = reservoir.wateruse_add = true;
    reservoir.calc_balance_var(ts, wateres::WATERUSE);
    double diff_yield = yield_req[ts] - reservoir.var[wateres::YIELD][ts];
    if (diff_yield > 0)
      reservoir.var[wateres::DEFICIT][ts] += diff_yield;
    double diff_withdrawal = reservoir.var[wateres::WATERUSE][ts] - withdrawal_req;
    if (diff_withdrawal > 0)
      reservoir.var[wateres::DEFICIT][ts] += diff_withdrawal;
    if (abs(reservoir.var[wateres::TRANSFER][ts]) > numeric_limits<double>::epsilon())
      is_transfer = true;
    if (till_deficit && ts >= first_deficit_pos && reservoir.var[wateres::DEFICIT][ts] > numeric_limits<double>::epsilon()) {
      time_steps = ts + 1;
    }
  }

  if (is_routing) {
    for (ts = initial_pos; ts < time_steps; ts++) {
      reservoir.var[wateres::YIELD_UNROUTED][ts] = reservoir.var[wateres::YIELD][ts];
      reservoir.var[wateres::YIELD][ts] = 0;
    }
    if (routing_method == "lag") {
      reservoir.calc_routing_lag(as<double>(routing_settings[0]), initial_pos, time_steps);
    }
    else if (routing_method == "linear_reservoir") {
      reservoir.calc_routing_linear_reservoir(as<double>(routing_settings[0]), initial_pos, time_steps);
    }
  }

  convert_m3(reservoir.var[wateres::YIELD], reservoir.minutes, false);
  if (is_routing) {
    convert_m3(reservoir.var[wateres::YIELD_UNROUTED], reservoir.minutes, false);
  }
  convert_m3(reservoir.var[wateres::INFLOW], reservoir.minutes, false);

  vector<double> resul_var;
  resul_var.resize(time_steps - initial_pos);

  vector<string> output_var_names = { "inflow", "storage", "yield", "precipitation", "evaporation", "wateruse", "deficit" };
  vector<wateres::var_name> output_vars = {
    wateres::INFLOW, wateres::YIELD, wateres::YIELD, wateres::PRECIPITATION, wateres::EVAPORATION,
    wateres::WATERUSE, wateres::DEFICIT };
  if (is_transfer) {
    output_var_names.push_back("transfer");
    output_vars.push_back(wateres::TRANSFER);
  }
  if (is_routing) {
    output_var_names.push_back("yield_unrouted");
    output_vars.push_back(wateres::YIELD_UNROUTED);
  }

  List resul;
  unsigned var_count = output_var_names.size();
  for (unsigned v = 0; v < var_count; v++) {
    for (ts = initial_pos; ts < time_steps; ts++) {
      if (v == 1)
        resul_var[ts - initial_pos] = reservoir.storage[ts + 1]; //initial storage not returned
      else
        resul_var[ts - initial_pos] = reservoir.var[output_vars[v]][ts];
    }
    resul[output_var_names[v]] = resul_var;
  }
  return resul;
}
