/* included  twice  from  ./split.cpp   after defining
    _L_INTSXP_, _L_INTEG_,  _L_int_
    ===========  =========  ========    as
      INTSXP  ,  INTEGER ,  int         or
      REALSXP ,  REAL    ,  R_xlen_t
*/
{
    PROTECT(counts = Rf_allocVector(_L_INTSXP_, nlevs));
    for (int i = 0; i < nlevs; i++) _L_INTEG_(counts)[i] = 0;
    R_xlen_t i, i1;
    MOD_ITERATE1(nobs, nfac, i, i1, {
	int j = INTEGER(f)[i1];
	if (j != R_NaInt) {
	    /* protect against malformed factors */
	    if (j > nlevs || j < 1) Rf_error(_("factor has bad level"));
	    _L_INTEG_(counts)[j - 1]++;
	}
    });
    /* Allocate a generic vector to hold the results. */
    /* The i-th element will hold the split-out data */
    /* for the ith group. */
    PROTECT(vec = Rf_allocVector(VECSXP, nlevs));
    for (R_xlen_t i = 0;  i < nlevs; i++) {
	SET_VECTOR_ELT(vec, i, 
		       Rf_allocVector(TYPEOF(x), (_L_int_)_L_INTEG_(counts)[i]));
	Rf_setAttrib(VECTOR_ELT(vec, i), Symbols::LevelsSymbol,
		  Rf_getAttrib(x, Symbols::LevelsSymbol));
	if(have_names)
	    Rf_setAttrib(VECTOR_ELT(vec, i), Symbols::NamesSymbol,
		      Rf_allocVector(STRSXP, (_L_int_)_L_INTEG_(counts)[i]));
    }
    for (int i = 0; i < nlevs; i++) _L_INTEG_(counts)[i] = 0;
    MOD_ITERATE1(nobs, nfac, i, i1, {
	int j = INTEGER(f)[i1];
	if (j != R_NaInt) {
	    _L_int_ k = (_L_int_)_L_INTEG_(counts)[j - 1];
	    switch (TYPEOF(x)) {
	    case LGLSXP:
	    case INTSXP:
		INTEGER(VECTOR_ELT(vec, j - 1))[k] = INTEGER(x)[i];
		break;
	    case REALSXP:
		REAL(VECTOR_ELT(vec, j - 1))[k] = REAL(x)[i];
		break;
	    case CPLXSXP:
		COMPLEX(VECTOR_ELT(vec, j - 1))[k] = COMPLEX(x)[i];
		break;
	    case STRSXP:
		SET_STRING_ELT(VECTOR_ELT(vec, j - 1), k, STRING_ELT(x, i));
		break;
	    case VECSXP:
		SET_VECTOR_ELT(VECTOR_ELT(vec, j - 1), k, VECTOR_ELT(x, i));
		break;
	    case RAWSXP:
		RAW(VECTOR_ELT(vec, j - 1))[k] = RAW(x)[i];
		break;
	    default:
		UNIMPLEMENTED_TYPE("split", x);
	    }
	    if(have_names) {
		nmj = Rf_getAttrib(VECTOR_ELT(vec, j - 1), Symbols::NamesSymbol);
		SET_STRING_ELT(nmj, k, STRING_ELT(nm, i));
	    }
	    _L_INTEG_(counts)[j - 1] += 1;
	}
    });
}
