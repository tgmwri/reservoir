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

/**
  * - calculates monthly time series of reservoir storage and yield
  * @param Rinflow time series of inflows in m3.s-1
  * @param Rdays number of days for months of time series
  * @param Ryield_req required yield (reservoir outflow) in m3.s-1
  * @param Rvolume reservoir potential volume in millions of m3
  */
RcppExport SEXP calc_storage(SEXP Rinflow, SEXP Rdays, SEXP Ryield_req, SEXP Rvolume)
{
  vector<double> inflow = as<vector<double> >(Rinflow);
  vector<unsigned> days = as<vector<unsigned> >(Rdays);
  double yield_req = as<double>(Ryield_req);
  double volume = as<double>(Rvolume);

  unsigned time_steps = inflow.size(), ts;
  vector<double> deltaQ(time_steps, 0);
  for (ts = 0; ts < time_steps; ts++) {
    deltaQ[ts] = inflow[ts] - yield_req;
  }
  convert_m3(deltaQ, days, true);
  vector<double> yield_req_vol(time_steps, yield_req);
  convert_m3(yield_req_vol, days, true);
  volume = volume * 1e6;

  vector<double> yield(time_steps, 0), storage(time_steps + 1, 0);
  storage[0] = volume; //reservoir considered full at the beginnig
  for (ts = 0; ts < time_steps; ts++) {
    yield[ts] = storage[ts] < yield_req_vol[ts] ? storage[ts] : yield_req_vol[ts];
    storage[ts + 1] = storage[ts] + deltaQ[ts];
    if (storage[ts + 1] < 0) {
      storage[ts + 1] = 0;
    }
    else if (storage[ts + 1] > volume) {
      yield[ts] = yield[ts] + storage[ts + 1] - volume;
      storage[ts + 1] = volume;
    }
  }
  List resul;
  resul["storage"] = storage;
  convert_m3(yield, days, false);
  resul["yield"] = yield;

  return resul;
}
