C
C  The following code replaces the original PF1 which gives slightly
C      inaccurate F values
C
      real function S (A, B, C, D)
      integer A, B, C, A1, B1
      S0 = 1
      S1 = 1
      A1 = A - 2
      B1 = B - 2
      do 10 J=B,C,2
      A1 = A1 + 2
      B1 = B1 + 2
      S1 = S1*A1/B1*D
      S0 = S0 + S1
   10 continue
      S = S0
      RETURN
      END
C
      real function LnFactor( N )
C
C    Calculates ln(N!) = ln( Gamma(N + 1) )
C
      G = 0
      do 10 J = 1,N
   10 G = G + LOG( REAL(J) )
      LnFactor = G
      RETURN
      END
C
      real function LnGammaO( V )
C
C    Calculates ln( Gamma(V + .5) ) when degrees of freedom are odd
C
      integer V
      Pi = 3.1415927
      G = LOG( SQRT(Pi) )
      do 10 J = INT( REAL(V)/2 - 1 + 0.5 ), 1, -1
   10 G = G + LOG( REAL(J) - 0.5)
      LnGammaO = G
      RETURN
      END
C
      logical function Odd( I )
      if (MOD(I,2).EQ.0) then
         Odd = .FALSE.
      else
         Odd = .TRUE.
      end if
      RETURN
      END
C
      subroutine pf1( F, NDFN, NDFD, P)
C Subroutine to calculate probabilities of F using algorithm in MICROSTAT
C
      integer V1, V2
      logical Odd
      real LnFactor, LnGammaO
      P1(X,V1,V2) = ( X**(REAL(V2)/2) )*S(V2,2,V1-2,1.0-X)
      P2(X,V1,V2) = 1 - ( (1.0-X)**(REAL(v1)/2) )* S(V1,2,V2-2,X)
      Pi = 3.1415927
      if (F.le.0) then
         P = 1.0
         return
      end if
      if (F.GE.1) then
          V1 = NDFN
          V2 = NDFD
          F0 = F
      else
          V1 = NDFD
          V2 = NDFN
          F0 = 1.0/F
      end if
      X = V2/(V2 + V1*F0)
      if ( Odd(V1).AND.Odd(V2) ) then
         AT = ATAN( SQRT( V1*F0/V2 ) )
         if (V2.GT.1) then
            Y = AT + SIN(AT) * COS(AT) * S( 2, 3, V2-2, COS(AT)**2 )
            Temp = 2/Pi*Y
         else
            Temp = 2*AT/Pi
         end if
         if (V1.GT.1) then
            Y = V2 * S( V2+1, 3, V1-2, SIN(AT)**2)
            P = 1 - Temp
     1             + EXP(
     2                  LOG( 2/SQRT(Pi) )
     3                  + LnFactor(  INT( (V2-1)/2 )  )
     4                  - LnGammaO( V2 )
     5                  + LOG(  SIN(AT)*( COS(AT)**Y )  )
     6                    )
         else
            P = 1 - Temp
         end if
      else if ( (.NOT.Odd(V1)).AND.(.NOT.Odd(V2)) )  then
              if (V1.LT.V2) then
                   P = P1( X, V1, V2)
              else
                   P = P2( X, V1, V2)
              end if
           else if ( Odd(V2) ) then
                   P = P1( X, V1, V2)
                else
                   P = P2( X, V1, V2)
                end if
      if (F.LT.1) P = 1.0 - P
      RETURN
      END
C
      program pcalc
C
      integer DFNum, DFDen
  101 format(\,1x,'Degrees of freedom for numerator (Enter/Return ',
     1 'to Exit)? ')
  102 format(\,1x,'Degrees of freedom for denominator? ')
  103 format(\,1x,'Value of F? ')
  104 format(\,1x,'Probability of F(', I3, ', ', I3, ') = ', F9.4,
     1 ' is ', F10.8, '.',/)
  201 format(I3)
  202 format(f9.4)
   10 write(*, 101)
      read(*, 201) DFNum
      if (DFNum.EQ.0) then
            STOP
         else
            write(*, 102)
            read(*, 201) DFDen
            write(*, 103)
            read *, F
      end if
      CALL PF1(F, DFNum, DFDen, P)
      write(*, 104) DFNum, DFDen, F, P
      goto 10
      END
