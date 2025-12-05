GRBL_Settings : dialog {
  label = "GRBL-Laser settings"; 
  : edit_box {                     // Specfiy the kerf-width of the laserbeam
    label = "&Laser kerf-width: ";
    key = "GRBL_Kerf";
    edit_width = 6;
  }
  
  : button {
    key = "OK";
    label = "OK";
    is_default = true;
    fixed_width = true;
    alignment = centered;
  }
  
}