/*
142 Sandu Anastasia
Code Blocks
Eduard szmeteanca*/
#include <iostream>
#include <exception>
#include <stdexcept>
#include <list>
#include <iterator>

using namespace std;


class CitireAfisare
{
public:
    virtual istream& Citire(istream& in)=0;
    virtual ostream& Afisare(ostream& out)const=0;
};
class Bilet: public CitireAfisare
{
protected:
    string tip;
    bool validat;
    const int idBilet;
    static int contorId;
public:
    Bilet();
    Bilet(string tip, bool validat);

    Bilet(const Bilet& bil);

    Bilet& operator=(const Bilet& bil);

    istream& Citire(istream& in);
    ostream& Afisare(ostream& out)const;

    friend istream& operator>>(istream& in, Bilet& bil);
    friend ostream& operator<<(ostream& out, const Bilet& bil);

    ~Bilet() {}
};
Bilet::Bilet():idBilet(contorId++)
{
    this->tip=" ";
    this->validat=0;
}
Bilet::Bilet(string tip, bool validat):idBilet(contorId++)
{
    this->tip=tip;
    this->validat=validat;
}
Bilet::Bilet(const Bilet& bil):idBilet(bil.idBilet)
{
    this->tip=bil.tip;
    this->validat=bil.validat;
}
Bilet& Bilet::operator=(const Bilet& bil)
{
    if(this!=&bil)
    {
        this->tip=bil.tip;
        this->validat=bil.validat;
    }
    return *this;
}
istream& Bilet::Citire(istream& in)
{
    cout<<"\nTipul ";
    try
    {
        in>>this->tip;
        string s=this->tip;
        std::string::const_iterator it=s.begin();
        while(it!=s.end() && isdigit(*it))
            ++it;
        if(it==s.end() && s.empty()==0)
            throw std::invalid_argument("invalid argument");
    }
    catch(invalid_argument)
    {
        cout<<"\nEste nevoie de un string";
    }
    catch(...)
    {
        cout<<"\nExceptie";
    }
    cout<<"\nIntroduceti 1 daca biletul este validat si 0 altfel ";
    in>>this->validat;
    return in;
}
ostream& Bilet::Afisare(ostream& out)const
{
    out<<"\nTipul "<<this->tip;
    if(this->validat==1)
        out<<"\nBiletul este validat ";
    else out<<"\nBiletul nu este validat";
    return out;
}
istream& operator>>(istream& in, Bilet& bil)
{
    return bil.Citire(in);
}
ostream& operator<<(ostream& out, const Bilet& bil)
{
    return bil.Afisare(out);
}
int Bilet::contorId=0;
class biletSuprafata: public Bilet
{
protected:
    double taxa;
public:
    biletSuprafata();
    biletSuprafata(double taxa, string tip, bool validat);

    biletSuprafata(const biletSuprafata& bil);

    biletSuprafata& operator=(const biletSuprafata& bil);

    istream& Citire(istream& in);
    ostream& Afisare(ostream& out)const;

    ~biletSuprafata() {}
};
biletSuprafata::biletSuprafata():Bilet()
{
    this->taxa=2;
}
biletSuprafata::biletSuprafata(double taxa, string tip, bool validat):Bilet(tip, validat)
{
    this->taxa=taxa;
}
biletSuprafata::biletSuprafata(const biletSuprafata& bil):Bilet(bil)
{
    this->taxa=bil.taxa;
}
biletSuprafata& biletSuprafata::operator=(const biletSuprafata& bil)
{
    if(this!=&bil)
    {
        this->taxa=bil.taxa;
    }
    return *this;
}
istream& biletSuprafata::Citire(istream& in)
{
    Bilet::Citire(in);
    cout<<"\nTaxa este ";
    in>>this->taxa;
    return in;
}
ostream& biletSuprafata::Afisare(ostream& out)const
{
    Bilet::Afisare(out);
    out<<"\nTaxa "<<this->taxa;
    return out;
}
class biletMetrou: public Bilet
{
protected:
    double taxa;
public:
    biletMetrou();
    biletMetrou(double taxa, string tip, bool validat);

    biletMetrou(const biletMetrou& bil);

    biletMetrou& operator=(const biletMetrou& bil);

    istream& Citire(istream& in);
    ostream& Afisare(ostream& out)const;

    ~biletMetrou() {}
};
biletMetrou::biletMetrou():Bilet()
{
    this->taxa=2.5;
}
biletMetrou::biletMetrou(double taxa, string tip, bool validat):Bilet(tip, validat)
{
    this->taxa=taxa;
}
biletMetrou::biletMetrou(const biletMetrou& bil):Bilet(bil)
{
    this->taxa=bil.taxa;
}
biletMetrou& biletMetrou::operator=(const biletMetrou& bil)
{
    if(this!=&bil)
    {
        this->taxa=bil.taxa;
    }
    return *this;
}
istream& biletMetrou::Citire(istream& in)
{
    Bilet::Citire(in);
    cout<<"\nTaxa este ";
    in>>this->taxa;
    return in;
}
ostream& biletMetrou::Afisare(ostream& out)const
{
    Bilet::Afisare(out);
    out<<"\nTaxa "<<this->taxa;
    return out;
}
class biletTranzit: public Bilet
{
protected:
    double taxa;
    int ora;
    int minut;
public:
    biletTranzit();
    biletTranzit(double taxa,int ora, int minut, string tip, bool validat);

    biletTranzit(const biletTranzit& bil);

    biletTranzit& operator=(const biletTranzit& bil);

    istream& Citire(istream& in);
    ostream& Afisare(ostream& out)const;

    ~biletTranzit() {}
};
biletTranzit::biletTranzit():Bilet()
{
    this->taxa=3;
    this->ora=0;
    this->minut=0;
}
biletTranzit::biletTranzit(double taxa,int ora, int minut, string tip, bool validat):Bilet(tip, validat)
{
    this->taxa=taxa;
    this->ora=ora;
    this->minut=minut;
}
biletTranzit::biletTranzit(const biletTranzit& bil):Bilet(bil)
{
    this->taxa=bil.taxa;
    this->ora=bil.ora;
    this->minut=bil.minut;
}
biletTranzit& biletTranzit::operator=(const biletTranzit& bil)
{
    if(this!=&bil)
    {
        this->taxa=bil.taxa;
        this->ora=bil.ora;
        this->minut=bil.minut;
    }
    return *this;
}
istream& biletTranzit::Citire(istream& in)
{
    Bilet::Citire(in);
    cout<<"\nTaxa este ";
    in>>this->taxa;
    cout<<"\nOra este ";
    in>>this->ora;
    cout<<"\nMinutul este ";
    in>>this->minut;
    return in;
}
ostream& biletTranzit::Afisare(ostream& out)const
{
    Bilet::Afisare(out);
    out<<"\nTaxa "<<this->taxa;
    out<<"\nOra "<<this->ora;
    out<<"\nMinutul "<<this->minut;
    return out;
}
class Card
{
protected:
    list<Bilet*>bilete;
public:
    Card();
    Card(list<Bilet*>bilete);

    Card(const Card& car);

    Card& operator=(const Card& car);


    friend istream& operator>>(istream& in, Card& car);
    friend ostream& operator<<(ostream& out, const Card& car);

    ~Card();
};
Card::Card()
{
    this->bilete={};
}
Card::Card(list<Bilet*>bilete)
{
    this->bilete=bilete;
}
Card& Card::operator=(const Card& car)
{
    if(this!=&car)
    {
        this->bilete=car.bilete;
    }
    return *this;
}
Card::Card(const Card& car)
{
    this->bilete=car.bilete;
}
istream& operator>>(istream& in, Card& car)
{
    cout<<"\nBilete de pe card sunt: \n";
    int k=1, comanda;
    while(k)
    {
        cout<<"\n1 pentru add bilet \n2 pentru stop\n";
        cin>>comanda;
        switch(comanda)
        {
        case 1:
        {
            int l=1,comandacitire;
            while(l)
            {
                cout<<"\nCe tip de card vreti sa introduceti? \n1 pentru bilet de suprafata \n2 pentru bilet de metrou \n3 back\n";
                cin>>comandacitire;
                switch(comandacitire)
                {
                case 1:
                {
                    biletSuprafata a;
                    cin>>a;
                    car.bilete.push_back(new biletSuprafata(a));
                    break;
                }
                case 2:
                {
                    biletMetrou a;
                    cin>>a;
                    car.bilete.push_back(new biletMetrou(a));
                    break;
                }
                case 3:
                {
                    l=0;
                    break;
                }
                default:
                {
                    cout<<"\nComanda gresita ";
                    break;
                }
                }

            }
            break;
        }
                case 2:
                    {
                        k=0;
                        break;
                    }
                default:
                    {
                        cout<<"\nComanda gresita";
                        break;
                    }
        }
    }
}
ostream& operator<<(ostream& out, const Card& car)
{
    out<<"\nBiletele sunt" ;
    list<Bilet*>s=car.bilete;
    list<Bilet*>::iterator it;
    for(it=s.begin(); it!=s.end(); it++)
        out<<*it<<endl;
    return out;
}
Card::~Card()
{
    if(this->bilete.empty()==0)
        this->bilete.clear();
}
class CardTranzit
{
protected:
    list<biletTranzit>bilete;
public:
    CardTranzit();
    CardTranzit(list<biletTranzit>bilete);

    CardTranzit(const CardTranzit& car);

    CardTranzit& operator=(const CardTranzit& car);

    friend CardTranzit operator+(biletTranzit& bil, CardTranzit car)
    {
        car.bilete.push_back(bil);
        return car;
    }
    friend CardTranzit operator+(CardTranzit car,biletTranzit& bil)
    {
        car.bilete.push_back(bil);
        return car;
    }

    friend istream& operator>>(istream& in, CardTranzit& car);
    friend ostream& operator<<(ostream& out, const CardTranzit& car);

    ~CardTranzit();
};
CardTranzit::CardTranzit()
{
    this->bilete= {};
}
CardTranzit::CardTranzit(list<biletTranzit>bilete)
{
    this->bilete=bilete;
}
CardTranzit& CardTranzit::operator=(const CardTranzit& car)
{
    if(this!=&car)
    {
        this->bilete=car.bilete;
    }
    return *this;
}
CardTranzit::CardTranzit(const CardTranzit& car)
{
    this->bilete=car.bilete;
}
CardTranzit::~CardTranzit()
{
    if(this->bilete.empty()==0)
        this->bilete.clear();
}
istream& operator>>(istream& in, CardTranzit& car)
{
    cout<<"\nIntroduceti biletele tranzit: ";
    int k=1,comanda;
    while(k)
    {
        cout<<"\n1 pentru add si 2 pentru stop\n";
        cin>>comanda;
        switch(comanda)
        {
        case 1 :
            {
                biletTranzit a;
                cin>>a;
                car.bilete.push_back(a);
                break;
            }
        case 2:
            {
                k=0;
                break;
            }
        default:
            {
                cout<<"\nComanda gresita";
                break;
            }
        }
    }
}
class Aparat
{
protected:
    int nrBileteValidate;
    int nrCarduriValidate;
public:
    Aparat();
    Aparat(int nrBileteValidate, int nrCarduriValidate);

    Aparat(const Aparat& ap);

    Aparat& operator=(const Aparat& ap);

    friend istream& operator>>(istream& in, Aparat& bil);
    friend ostream& operator<<(ostream& out, const Aparat& bil);

    ~Aparat(){}
};
Aparat::Aparat()
{
    this->nrBileteValidate=0;
    this->nrCarduriValidate=0;
}
Aparat::Aparat(int nrBileteValidate, int nrCarduriValidate)
{
    this->nrBileteValidate=nrBileteValidate;
    this->nrCarduriValidate=nrCarduriValidate;
}
Aparat::Aparat(const Aparat& ap)
{
    this->nrBileteValidate=ap.nrBileteValidate;
    this->nrCarduriValidate=ap.nrCarduriValidate;
}
Aparat& Aparat::operator=(const Aparat& ap)
{
    if(this!=&ap)
    {
        this->nrBileteValidate=ap.nrBileteValidate;
        this->nrCarduriValidate=ap.nrCarduriValidate;
    }
    return *this;
}
istream& operator>>(istream& in, Aparat& bil)
{
    cout<<"\nNumarul de bilete este ";
    in>>bil.nrBileteValidate;
    cout<<"\nNumarul cardurilor este ";
    in>>bil.nrCarduriValidate;
    return in;
}
ostream& operator<<(ostream& out, const Aparat& bil)
{
    out<<"\nNR de bilete :";
    out<<bil.nrBileteValidate;
    out<<"\nNr carduri ";
    out<<bil.nrCarduriValidate;
    return out;
}
class meniuInteractiv
{
private:
    static meniuInteractiv* obiect;
    string data;
    meniuInteractiv()
    {
        this->data="";
    }
    meniuInteractiv(const meniuInteractiv& men){}
    meniuInteractiv& operator=(const meniuInteractiv& men){}
    ~meniuInteractiv()
    {
        if(this->obiect!=NULL)
            delete this->obiect;
    }
public:
    static meniuInteractiv* getInstance()
    {
        if(!obiect)
            obiect=new meniuInteractiv;
        return obiect;
    }
    void meniu();
};

int main()
{
    cout << "Hello world!" << endl;
    return 0;
}
