#lang grace



def o = object {
   method m() {
      return 1
   }
   
   def x = object {
      def a = m()
   }
}


