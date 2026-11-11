	subroutine isbyte(k,buf,j)
c		needed in subroutine trnslt.
	character*1 buf(*)
        buf(j+1)=char(k)
 	return
	end
