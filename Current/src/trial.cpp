// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <RcppArmadilloExtensions/sample.h>

using namespace Rcpp;

// Chromosome data storage
struct chromosome
{
  DataFrame runDat;
  double Ins;
  double count;
  unsigned int fitness;
};

// Function to generate new values of i_number and insertion
double roll(IntegerVector set, double oldval) {
    double newval;
    NumericVector newvect;
    double _oldval = oldval;
    bool replace = FALSE;
    NumericVector prob = NumericVector::create();
    IntegerVector _set = set;

    newvect = RcppArmadillo::sample(_set, 1, replace, prob);
    newval = newvect[0];

    if (!_oldval)
        return newval;
    while (newval <= _oldval) {
        newvect = RcppArmadillo::sample(_set, 1, replace, prob);
        newval = newvect[0];
    }
    return newval;
}

// Function to get fitness of model run
double getFit(DataFrame run) {
  DataFrame _run;
  Environment global = Environment::global_env();
  Function check = global["edtrial"];
  double fitness;

  fitness = check(_run);
  return fitness;
}

// [[Rcpp::export]]
NumericVector trialrun(Environment env, String age) {
    // Run batch run function and iterate
    Environment global = Environment::global_env();
    Environment _env = env;
    String _age = age;

    const int tf = 3650;

    Function runBatch = global["brtrial"];
    Function epiCheck = global["edtrial"];

    // Some parameter sets here
    IntegerVector infCount = seq_len(100);
    IntegerVector insertion = seq_len(3000);
    const int occ = 12;
    double newCount, newIns, oldCount = 0, oldIns = 0;
    NumericVector out(3);
    out.names() = CharacterVector::create("Insertion Time",
                                          "Infected Count",
                                          "Occurances");

    bool successone;
    do {
        newIns = roll(insertion, oldIns);
        newCount = roll(infCount, oldCount);
        // Rewrite whole thing?
        DataFrame run = runBatch(_env, newIns, newCount, occ, _age, tf);
        successone = epiCheck(run);
    } while (!successone);

    // First Sucesss
    bool successtwo = 1;
    bool replace = FALSE;
    NumericVector newvect(0);
    out["Insertion Time"] = newIns;
    out["Infected Count"] = newCount;
    out["Occurances"] = occ;
    oldCount = out["Infected Count"];
    IntegerVector range = seq_len(out["Infected Count"]);

    // Now decrease number of people being inserted to optimize
    do {
        newvect = RcppArmadillo::sample(range, 1, replace,
                                        NumericVector::create());
        newCount = newvect[0];
        if (newCount >= oldCount)
            continue;
        DataFrame run = runBatch(env, out["Insertion Time"], newCount,
                                 out["Occurances"], _age, tf);
        successtwo = epiCheck(run);
        if (successtwo)
            oldCount = newCount;
    } while (successtwo);
    // Second Success
    // Store the minimized Count data in output vector
    out[1] = newCount;


    // Return output

    return out;
}