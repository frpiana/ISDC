# Main class definition
sample_EIS <- setRefClass(
  "sample_EIS", # Class name
  fields = list(
    code = "character",
    dataSource = "character",
    thickness = "numeric",
    diameter = "numeric",
    area = "numeric",
    dielectric = "data.frame"
  ),
  methods = list(
    # Init method to asign the object parameters to part of the fields and calculate the rest
    initialize = function(code, dataSource, diameter, thickness) {
      .self$code = code
      .self$diameter = diameter
      .self$dataSource = dataSource
      .self$thickness = thickness
      .self$area = pi*(.self$diameter/2)^2
      .self$dataImport() # Call to the method for the import of standard ASCII files
      .self$dataCalc() # Call to the method for the calculation of the dielectric parameters
    }
  )
)

sample_EIS$methods(
  # Method for the import of standard ASCII files
  dataImport = function(){
    .self$dielectric <<- readr::read_delim(
      dataSource,
      delim = "\t",
      escape_double = FALSE,
      trim_ws = TRUE,
      skip = 3)
    names(.self$dielectric) <- c("freq",
                                 "r_p_impedance",
                                 "i_p_impedance",
                                 "r_s_impedance",
                                 "i_s_impedance")
  }
  
)

sample_EIS$methods(
  # method for the calculation of the dielectric parameters
  dataCalc = function(){
    dielectric$r_s_capacitance <<- 1/(2*pi*dielectric$freq*dielectric$i_s_impedance)
    dielectric$i_s_capacitance <<- 1/(2*pi*dielectric$freq*dielectric$r_s_impedance)
    dielectric$m_s_capacitance <<- sqrt(dielectric$r_s_capacitance^2 + dielectric$i_s_capacitance^2)
    
    dielectric$r_p_capacitance <<- 1/(2*pi*dielectric$freq*dielectric$i_p_impedance)
    dielectric$i_p_capacitance <<- 1/(2*pi*dielectric$freq*dielectric$r_p_impedance)
    dielectric$m_p_capacitance <<- sqrt(dielectric$r_p_capacitance^2 + dielectric$i_p_capacitance^2)
    
    dielectric$r_s_admittance <<- 1/dielectric$r_s_impedance
    dielectric$i_s_admittance <<- 1/dielectric$i_s_impedance
    dielectric$m_s_admittance <<- sqrt(dielectric$r_s_admittance^2 + dielectric$i_s_admittance^2)
    
    dielectric$r_p_admittance <<- 1/dielectric$r_p_impedance
    dielectric$i_p_admittance <<- 1/dielectric$i_p_impedance
    dielectric$m_p_admittance <<- sqrt(dielectric$r_p_admittance^2 + dielectric$i_p_admittance^2)
    
    dielectric$r_permittivity <<- dielectric$r_p_capacitance*.self$thickness/(.self$area*8.8541878128e-12)
    dielectric$i_permittivity <<- 1/(dielectric$r_p_capacitance*dielectric$freq)
    
    dielectric$r_conductivity <<- 2*pi*dielectric$freq*dielectric$i_permittivity*8.8541878128e-12/100
    dielectric$i_conductivity <<- -2*pi*dielectric$freq*8.8541878128e-12*(dielectric$r_permittivity - 1)/100
    
    dielectric$r_modulus <<- dielectric$r_permittivity/(dielectric$r_permittivity^2 + dielectric$i_permittivity^2)
    dielectric$i_modulus <<- dielectric$i_permittivity/(dielectric$r_permittivity^2 + dielectric$i_permittivity^2)
    
    dielectric$r_p_inductance <<- -dielectric$i_p_impedance/dielectric$freq
    dielectric$i_p_inductance <<- -dielectric$r_p_impedance/dielectric$freq
    
    dielectric$r_s_inductance <<- -dielectric$i_s_impedance/dielectric$freq
    dielectric$i_s_inductance <<- -dielectric$r_s_impedance/dielectric$freq
    
    dielectric$tangent_delta <<- dielectric$r_s_capacitance/dielectric$i_s_capacitance
    dielectric$phase_angle <<- 180*atan(1/dielectric$tangent_delta)/pi
  }
)


