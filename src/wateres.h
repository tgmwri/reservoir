#ifndef WATERES_H_INCLUDED
#define WATERES_H_INCLUDED

/**
 * - reservoir variables and options
 */
class wateres
{
  public:
    wateres(Rcpp::DataFrame reser, std::vector<double> storage, bool throw_exceed, double volume); //!< reservoir from given values

    //! water balance variables
    enum var_name {INFLOW, EVAPORATION, WITHDRAWAL, PRECIPITATION, YIELD};
    static const unsigned var_count = 5; //!< number of time series of variables
    static const std::string var_names[]; //!< names of variables
    std::vector<std::vector<double> > var; //!< values of water balance variables
    std::vector<double> storage;
    std::vector<unsigned> days; //!< number of days for time steps
    Rcpp::DataFrame eas; //!< elevation-area-storage relationship (m, m2 and m3)

    void calc_balance_var(unsigned ts, var_name var); //!< calculates water balance for given variable and time step

  private:
    bool throw_exceed; //!< whether volume exceeding maximum storage will be thrown
    double volume; //!< maximum reservoir storage
    double area; //!< flooded area for maximum storage

    double get_area(double storage_req); //!< gets area for given storage
};

#endif // WATERES_H_INCLUDED
