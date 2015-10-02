#include <Rcpp.h>

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
  * - calculates monthly time series of reservoir storage and yield
  * @param Rinflow time series of inflows in m3.s-1
  * @param Rdays number of days for months of time series
  * @param Revaporation time series of evaporation in mm
  * @param Ryield_req required yield (reservoir outflow) in m3.s-1
  * @param Rvolume reservoir potential volume in millions of m3
  * @param Rarea area flooded by reservoir in km2
  * @param Rthrow_exceed whether volume exceeding maximum storage will be thrown or added to yield
  * @return list consisting of storage (in m3), yield (m3.s-1) and evaporation (m3)
  */
RcppExport SEXP calc_storage(SEXP Rinflow, SEXP Rdays, SEXP Revaporation, SEXP Ryield_req, SEXP Rvolume, SEXP Rarea, SEXP Rthrow_exceed)
{
  vector<double> inflow = as<vector<double> >(Rinflow);
  vector<unsigned> days = as<vector<unsigned> >(Rdays);
  vector<double> evaporation = as<vector<double> >(Revaporation);
  double yield_req = as<double>(Ryield_req);
  double volume = as<double>(Rvolume);
  double area = as<double>(Rarea);
  bool throw_exceed = as<bool>(Rthrow_exceed);

  unsigned time_steps = inflow.size(), ts;
  for (ts = 0; ts < time_steps; ts++) {
    evaporation[ts] = convert_mm_to_m3s1(evaporation[ts], days[ts], area);
  }
  convert_m3(inflow, days, true);
  convert_m3(evaporation, days, true);
  vector<double> yield_req_vol(time_steps, yield_req);
  convert_m3(yield_req_vol, days, true);
  volume = volume * 1e6;

  vector<double> yield(time_steps, 0), storage(time_steps + 1, 0);
  storage[0] = volume; //reservoir considered full at the beginning
  for (ts = 0; ts < time_steps; ts++) {
    yield[ts] = yield_req_vol[ts];
    storage[ts + 1] = storage[ts] + inflow[ts];
    storage[ts + 1] -= evaporation[ts];
    if (storage[ts + 1] < 0) {
      evaporation[ts] = evaporation[ts] + storage[ts + 1];
      storage[ts + 1] = 0;
      yield[ts] = 0;
    }
    else {
      storage[ts + 1] -= yield[ts];
      if (storage[ts + 1] < 0) {
        yield[ts] = yield[ts] + storage[ts + 1];
        storage[ts + 1] = 0;
      }
      else if (storage[ts + 1] > volume) {
        if (!throw_exceed)
          yield[ts] += storage[ts + 1] - volume;
        storage[ts + 1] = volume;
      }
    }
  }
  List resul;
  resul["storage"] = storage;
  convert_m3(yield, days, false);
  resul["yield"] = yield;
  resul["evaporation"] = evaporation;

  return resul;
}
