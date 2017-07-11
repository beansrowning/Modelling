#include <Rcpp.h>

using namespace Rcpp;

// Function to generate new values of i_number and insertion
double roll(NumericVector set double oldval) {
    double newval;
    
    if (!oldval)
        newval = R::sample(set, 1);
        return newval;
    do {
        newval = R::sample(set, 1);
    } while (newval <= oldIns);
    return newval;
}

// [[Rcpp::export]]
NumericVector trialrun(Environment env String age) {
    // Run batch run function and iterate
    Environment global = Environment::global_env;
    // NumericVector .inits = env["init.values"];
    // NumericVector .params = env["parameters"];
    // NumericVector .transitions = env["transitions"];
    // Function .RateF = env["RateF"];
    double tf = 3650;
    // TODO: Create both of these functions
    Function runBatch = global["brtrial"];
    Function epiCheck = global["edtrial"]; 
    
    // Some parameter sets here
    NumericVector infCount = R::seq(1,100);
    NumericVector insertion = R::seq(0,3000);
    NumericVector occ = R::seq(1,120);
    double newCount, newIns, newOcc, oldCount, oldIns, oldOcc;
    NumericVector out(2);
    out.name = CharacterVector::create("Insertion Time", 
                                       "Infected Count", 
                                       "Occurances");
    
    bool successone;
    oldIns, oldCount, oldOcc = 0;
    do {
        newIns = roll(insertion, oldIns);
        newCount = roll(infCount, oldCount);
        newOcc = oldOcc++;
        // Rewrite whole thing?  
        DataFrame run = runBatch(env, insertion = newIns, i_number = newCount, 
                                 occ = newOcc, grp = age, length = tf); 
        successone = epicheck(run);
    } while (!successone);
    
    // First Sucesss
    bool successtwo = 1;
    out[0] = newIns;
    out[1] = newCount;
    out[2] = newOcc;
    double oldCount = out[1]; 
    // Now decrease number of people being inserted to optimize
    do {
        newCount = R::runif(min = 1, max = out[1]);
        if (newCount >= oldCount)
            continue;
        DataFrame run = runBatch(env, insertion = out[0], i_number = newCount, 
        occ = out[2], grp = age, length = tf); 
        successtwo = epicheck(run);
        if (successtwo)
            oldCount = newCount;
    } while (successtwo);
    // Second Success 
    // Store the minimized Count data in output vector
    out[1] = newCount;
    
    
    // Return output
    
    return out;
}