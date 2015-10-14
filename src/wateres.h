#ifndef WATERES_H_INCLUDED
#define WATERES_H_INCLUDED

/**
 * - reservoir variables and options
 */
class wateres
{
  public:
    wateres(
      std::vector<double> evaporation, std::vector<double> withdrawal, std::vector<double> yield, std::vector<double> storage,
      std::vector<unsigned> days, Rcpp::DataFrame eas, bool throw_exceed, double volume, double area); //!< reservoir from given values

    //! water balance variables
    enum var_name {EVAPORATION, YIELD, WITHDRAWAL};
    //! values of water balance variables
    std::vector<double> evaporation, withdrawal, yield, storage;
    std::vector<unsigned> days; //!< number of days for time steps
    Rcpp::DataFrame eas; //!< elevation-area-storage relationship (m, m2 and m3)

    void calc_balance_var(std::vector<double> &variable, unsigned ts, var_name var); //!< calculates water balance for given variable and time step

  private:
    bool throw_exceed; //!< whether volume exceeding maximum storage will be thrown
    double volume; //!< maximum reservoir storage
    double area; //!< flooded area for maximum storage

    double get_area(double storage_req); //!< gets area for given storage
};

#endif // WATERES_H_INCLUDED
