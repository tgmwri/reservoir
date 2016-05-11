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
    enum var_name {INFLOW, EVAPORATION, WATERUSE, PRECIPITATION, YIELD, DEFICIT, TRANSFER};
    static const unsigned var_count = 7; //!< number of time series of variables
    static const std::string var_names[]; //!< names of variables
    std::vector<std::vector<double> > var; //!< values of water balance variables
    std::vector<double> storage;
    std::vector<unsigned> minutes; //!< number of minutes for time steps
    Rcpp::DataFrame eas; //!< elevation-area-storage relationship (m, m2 and m3)
    bool transfer_add; //!< whether transfer variable will be added (beginning of reservoir balance) or removed (end)
    bool wateruse_add;

    void calc_balance_var(unsigned ts, var_name var); //!< calculates water balance for given variable and time step

  private:
    bool throw_exceed; //!< whether volume exceeding maximum storage will be thrown
    double volume; //!< maximum reservoir storage
    double area; //!< flooded area for maximum storage

    double get_area(double storage_req); //!< gets area for given storage
    void set_var_zero(unsigned ts, var_name var_n); //!< sets values of variables in a time step to zero
    bool check_value_sign(unsigned ts, var_name var_n, var_name next_var_n, bool &adding); //!< handles if variable value has corresponding sign
};

#endif // WATERES_H_INCLUDED
