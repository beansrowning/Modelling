// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <R.h>
#include <RcppArmadilloExtensions/sample.h>

using namespace Rcpp;

// Function to generate new values of i_number and insertion
double roll(IntegerVector set, double oldval) {
    double newval;
    NumericVector newvect;
    double _oldval = oldval;
    bool replace = FALSE;
    NumericVector prob = NumericVector::create();
    IntegerVector _set;

    if (!_oldval)
        newvect = RcppArmadillo::sample(_set, 1, replace, prob);
        newval = newvect(0);
        return newval;
    do {
        newvect = RcppArmadillo::sample(_set, 1, replace, prob);
        newval = newvect(0);
    } while (newval <= _oldval);
    return newval;
}

// [[Rcpp::export]]
NumericVector trialrun(Environment env, String age) {
    // Run batch run function and iterate
    Environment global = Environment::global_env();
    Environment _env = env;
    String _age = age;
    // NumericVector .inits = env["init.values"];
    // NumericVector .params = env["parameters"];
    // NumericVector .transitions = env["transitions"];
    // Function .RateF = env["RateF"];
    double tf = 3650;
    // TODO: Create both of these functions
    Function runBatch = global["brtrial"];
    Function epiCheck = global["edtrial"];

    // Some parameter sets here
    IntegerVector infCount = seq(1,100);
    IntegerVector insertion = seq(0,3000);
    IntegerVector occ = seq(1,120);
    double newCount, newIns, newOcc, oldCount, oldIns, oldOcc;
    NumericVector out(2);
    out.names() = CharacterVector::create("Insertion Time",
                                       "Infected Count",
                                       "Occurances");

    bool successone;
    oldIns = 0, oldCount = 0, oldOcc = 0;
    do {
        newIns = roll(insertion, oldIns);
        newCount = roll(infCount, oldCount);
        newOcc = oldOcc++;
        // Rewrite whole thing?
        DataFrame run = runBatch(_env, newIns, newCount, newOcc, _age, tf);
        successone = epiCheck(run);
    } while (!successone);

    // First Sucesss
    bool successtwo = 1;
    NumericVector newvect(0);
    out[0] = newIns;
    out[1] = newCount;
    out[2] = newOcc;
    oldCount = out[1];

    // Now decrease number of people being inserted to optimize
    do {
        newvect = runif(1, 1, out[1]);
        newCount = newvect(0);
        if (newCount >= oldCount)
            continue;
        DataFrame run = runBatch(env, out[0], newCount, out[2], _age, tf);
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
