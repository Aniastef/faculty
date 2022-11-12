class Main{
    public static void main(String[] args)
    {
	Tir t=new Tir();
	Remorca r1=new Remorca(5,"Remorca1");
	Remorca r2=new Remorca(20,"Remorca2");
	Remorca r3=new Remorca(12,"Remorca3");
	
	System.out.println(r1);
	t.adaugaRemorca(r1);
	t.adaugaRemorca(r2);
	t.adaugaRemorca(r3);
	t.stergeRemorca("Remorca2");
	System.out.println(t);

	Tir t2=new Tir();
	t2.adaugaRemorca(17,"altaRemorca");
	if(!t.equals(t2))
	    System.out.println(t2);
    }
    
}

class Tir{
    private Remorca[] r=new Remorca[5];
    int index=0;
   

    public boolean adaugaRemorca(int nrCutii, String nrInmatriculare)
    {
	if(index==r.length-1)
	    return false;
	else
	    {
		r[index]=new Remorca(nrCutii,nrInmatriculare);
		index++;
		return true;
	    }
    }

    public boolean adaugaRemorca(Remorca a)
    {
	if(index==r.length-1)
	    return false;
	else
	    {
		r[index]=new Remorca(a.nrCutii,a.nrInmatriculare);
		index++;
		return true;
	    }
    }

    
    public Remorca stergeRemorca(String nr)
    {
	int poz=0;
	int p=0;
	int i;
	Remorca ref=null;
	
	for(i=0;i<index;i++)
	    {
		if(r[i].nrInmatriculare.equals(nr))
		    {
			poz=i;
			p=1;
			ref=r[i];
			
		    }
	    }
	
	if(p==1)
	    {
		for(i=poz;i<index-1;i++)
	    {
		r[i]=r[i+1];
	    }
		index--;
		return ref;
	    }
	else
	    return null;
    }

    public boolean equals(Object t)
    {
	int c1=0;
	int c2=0;
	
	Tir t2=(Tir)t;

	for(int i=0;i<this.index;i++)
	    {
		c1=this.r[i].nrCutii+c1;
	    }
	
	for(int i=0;i<t2.index;i++)
	    {
		c2=t2.r[i].nrCutii+c2;
	    }

	return c1==c2;
    }
    
    public String toString()
    {
	String s="T";
	for (int i=0;i<index;i++)
	    {
		s=s+"->"+r[i].toString();
	    }
	return s;
    }
}

    
		

class Remorca{
    public int nrCutii;
    public String nrInmatriculare;
    private Remorca anterior=null;
    
   

    public Remorca(int nrCutii, String nrInmatriculare)
    {
	this.nrCutii=nrCutii;
	this.nrInmatriculare=nrInmatriculare;
	anterior=this;
    }

    public Remorca(String nrInmatriculare)
    {
	if(anterior==null)
	    {
		nrCutii=10;
		anterior=this;
	    }
	else
	    {
		nrCutii=anterior.nrCutii+1;
		this.nrInmatriculare=nrInmatriculare;
		anterior=this;
	    }
    }

    public String toString()
    {
	return "R(" + nrInmatriculare + ")(" + nrCutii+")";
    }

}

    
