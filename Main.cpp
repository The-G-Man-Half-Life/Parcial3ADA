//Include of the required tools
#include <algorithm>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <ctime>
#include <fstream>
#include <iostream>
#include <map>
#include <queue>
#include <random>
#include <set>
#include <sstream>
#include <string>
#include <utility>
#include <vector>

using namespace std;

const int VENTANA_DIAS = 30;  
const int K = 5;              
const int D = 3;              
const double ALFA = 0.5;      
const string ARCHIVO_LIBROS = "libros.csv";
const string ARCHIVO_PRESTAMOS = "prestamos.csv";


long diasDesdeCivil(int y, int m, int d) {
    y -= m <= 2;
    long era = (y >= 0 ? y : y - 399) / 400;
    long yoe = y - era * 400;
    long doy = (153 * (m + (m > 2 ? -3 : 9)) + 2) / 5 + d - 1;
    long doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
    return era * 146097 + doe - 719468;
}

string fechaISO(long z) {
    z += 719468;
    long era = (z >= 0 ? z : z - 146096) / 146097;
    long doe = z - era * 146097;
    long yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365;
    long y = yoe + era * 400;
    long doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    long mp = (5 * doy + 2) / 153;
    long d = doy - (153 * mp + 2) / 5 + 1;
    long m = mp + (mp < 10 ? 3 : -9);
    y += (m <= 2);
    char buf[64];
    snprintf(buf, sizeof(buf), "%04ld-%02ld-%02ld", y, m, d);
    return string(buf);
}

bool parseFecha(const string& s, long& dias) {
    int y, m, d;
    if (sscanf(s.c_str(), "%d-%d-%d", &y, &m, &d) != 3) return false;
    dias = diasDesdeCivil(y, m, d);
    return true;
}

long hoy() { return (long)(time(NULL) / 86400); }

vector<string> dividir(const string& linea, char sep) {
    vector<string> partes;
    string actual;
    for (size_t i = 0; i < linea.size(); i++) {
        char c = linea[i];
        if (c == sep) { partes.push_back(actual); actual.clear(); }
        else if (c != '\r' && c != '\n') actual += c;
    }
    partes.push_back(actual);
    return partes;
}

string aMayus(string s) {
    for (size_t i = 0; i < s.size(); i++) s[i] = (char)toupper((unsigned char)s[i]);
    return s;
}

string rellenar(const string& s, int ancho) {
    int n = 0;
    for (size_t i = 0; i < s.size(); i++)
        if (((unsigned char)s[i] & 0xC0) != 0x80) n++;
    return s + string(n < ancho ? ancho - n : 0, ' ');
}

struct Libro {
    string id, titulo, autor, categoria;
    int paginas, anio;
    int totalPrestamos;       
    int copiasDisponibles;
    map<string, int> vecinos; 

    Libro() : paginas(0), anio(0), totalPrestamos(0), copiasDisponibles(0) {}
    Libro(const string& i, const string& t, const string& a, const string& c,
          int p, int an, int cop)
        : id(i), titulo(t), autor(a), categoria(c), paginas(p), anio(an),
          totalPrestamos(0), copiasDisponibles(cop) {}

    int pesoSalienteTotal() const {
        int s = 0;
        for (map<string, int>::const_iterator it = vecinos.begin(); it != vecinos.end(); ++it)
            s += it->second;
        return s;
    }
};

struct Recomendacion {
    const Libro* libro;
    double afinidad;
    string motivo;
};

class Biblioteca {
public:
    map<string, Libro> grafo;                              
    map<string, pair<string, long> > ultimoPrestamo;        
    map<string, vector<pair<string, long> > > historial;   
    set<pair<string, string> > activos;                    

    bool agregarLibro(const Libro& l) {
        if (grafo.count(l.id)) return false;
        grafo[l.id] = l;
        return true;
    }

    bool cargarLibros(const string& ruta) {
        ifstream f(ruta.c_str());
        if (!f) return false;
        string linea;
        getline(f, linea);  
        while (getline(f, linea)) {
            vector<string> c = dividir(linea, ';');
            if (c.size() < 7) continue;
            agregarLibro(Libro(c[0], c[1], c[2], c[3], atoi(c[4].c_str()),
                               atoi(c[5].c_str()), atoi(c[6].c_str())));
        }
        return true;
    }

    bool cargarPrestamos(const string& ruta) {
        ifstream f(ruta.c_str());
        if (!f) return false;
        vector<pair<long, pair<string, string> > > filas;
        string linea;
        getline(f, linea); 
        while (getline(f, linea)) {
            vector<string> c = dividir(linea, ';');
            long fecha;
            if (c.size() < 3 || !parseFecha(c[2], fecha)) continue;
            filas.push_back(make_pair(fecha, make_pair(c[0], c[1])));
        }
        sort(filas.begin(), filas.end());  
        for (size_t i = 0; i < filas.size(); i++) {
            string msg;
            registrarPrestamo(filas[i].second.first, filas[i].second.second,
                              filas[i].first, false, msg);
        }
        return true;
    }

    void generarPrestamosSimulados(const string& ruta, int nUsuarios = 60, unsigned semilla = 2024) {
        mt19937 rng(semilla);
        vector<string> ids;
        map<string, vector<string> > porTema;
        for (map<string, Libro>::iterator it = grafo.begin(); it != grafo.end(); ++it) {
            ids.push_back(it->first);
            porTema[it->second.categoria].push_back(it->first);
        }
        vector<double> pesos;  
        for (size_t i = 0; i < ids.size(); i++) {
            vector<string>& lista = porTema[grafo[ids[i]].categoria];
            int pos = (int)(find(lista.begin(), lista.end(), ids[i]) - lista.begin());
            pesos.push_back(max(1, 10 - pos));
        }
        discrete_distribution<int> dist(pesos.begin(), pesos.end());
        uniform_real_distribution<double> u01(0.0, 1.0);
        ofstream f(ruta.c_str());
        f << "usuario;id_libro;fecha\n";
        long base = diasDesdeCivil(2026, 1, 5);
        for (int u = 1; u <= nUsuarios; u++) {
            char buf[16];
            snprintf(buf, sizeof(buf), "U%03d", u);
            string usuario(buf);
            string actual = ids[dist(rng)];
            long fecha = base + uniform_int_distribution<int>(0, 200)(rng);
            set<string> leidos;
            int n = uniform_int_distribution<int>(6, 12)(rng);
            for (int i = 0; i < n; i++) {
                f << usuario << ";" << actual << ";" << fechaISO(fecha) << "\n";
                leidos.insert(actual);
                vector<string>& lista = porTema[grafo[actual].categoria];
                int p = (int)(find(lista.begin(), lista.end(), actual) - lista.begin());
                string sig = actual;
                for (int intento = 0; intento < 10; intento++) {
                    double r = u01(rng);
                    if (r < 0.60)
                        sig = lista[(p + uniform_int_distribution<int>(1, 2)(rng)) % lista.size()];
                    else if (r < 0.85)
                        sig = lista[uniform_int_distribution<int>(0, (int)lista.size() - 1)(rng)];
                    else
                        sig = ids[uniform_int_distribution<int>(0, (int)ids.size() - 1)(rng)];
                    if (!leidos.count(sig)) break;
                }
                actual = sig;
                fecha += uniform_int_distribution<int>(3, 20)(rng);
            }
        }
    }
    bool registrarPrestamo(const string& usuario, const string& idLibro, long fecha,
                           bool enCurso, string& msg) {
        map<string, Libro>::iterator it = grafo.find(idLibro);
        if (it == grafo.end()) {
            msg = "El libro '" + idLibro + "' no existe.";
            return false;
        }
        Libro& libro = it->second;
        if (enCurso) {
            if (libro.copiasDisponibles <= 0) {
                msg = "No hay copias disponibles de '" + libro.titulo + "'.";
                return false;
            }
            if (activos.count(make_pair(usuario, idLibro))) {
                msg = "El usuario ya tiene prestado ese libro.";
                return false;
            }
        }
        map<string, pair<string, long> >::iterator prev = ultimoPrestamo.find(usuario);
        if (prev != ultimoPrestamo.end()) {
            const string& idPrev = prev->second.first;
            long dias = fecha - prev->second.second;
            if (idPrev != idLibro && dias >= 0 && dias <= VENTANA_DIAS)
                grafo[idPrev].vecinos[idLibro] += 1;  
        }
        libro.totalPrestamos++;
        if (enCurso) {
            libro.copiasDisponibles--;
            activos.insert(make_pair(usuario, idLibro));
        }
        ultimoPrestamo[usuario] = make_pair(idLibro, fecha);
        historial[usuario].push_back(make_pair(idLibro, fecha));
        msg = "Prestamo registrado: '" + libro.titulo + "' -> " + usuario + ".";
        return true;
    }

    bool registrarDevolucion(const string& usuario, const string& idLibro, string& msg) {
        pair<string, string> clave = make_pair(usuario, idLibro);
        if (!activos.count(clave)) {
            msg = "Ese usuario no tiene un prestamo activo de ese libro.";
            return false;
        }
        activos.erase(clave);
        grafo[idLibro].copiasDisponibles++;
        msg = "Devolucion registrada.";
        return true;
    }

    vector<const Libro*> masPopulares(int n) const {
        vector<const Libro*> v;
        for (map<string, Libro>::const_iterator it = grafo.begin(); it != grafo.end(); ++it)
            v.push_back(&it->second);
        sort(v.begin(), v.end(), [](const Libro* a, const Libro* b) {
            if (a->totalPrestamos != b->totalPrestamos) return a->totalPrestamos > b->totalPrestamos;
            return a->titulo < b->titulo;
        });
        if ((int)v.size() > n) v.resize(n);
        return v;
    }

    vector<Recomendacion> recomendar(const string& usuario, const string& origen,
                                     int k = K, int d = D, double alfa = ALFA) const {
        set<string> yaLeidos;
        map<string, vector<pair<string, long> > >::const_iterator h = historial.find(usuario);
        if (h != historial.end())
            for (size_t i = 0; i < h->second.size(); i++) yaLeidos.insert(h->second[i].first);

        map<string, double> afinidad;
        map<string, int> nivel;
        afinidad[origen] = 1.0;
        nivel[origen] = 0;
        queue<string> cola;
        cola.push(origen);
        while (!cola.empty()) {
            string u = cola.front();
            cola.pop();
            if (nivel[u] == d) continue;
            const Libro& lu = grafo.find(u)->second;
            int total = lu.pesoSalienteTotal();
            if (total == 0) continue;
            for (map<string, int>::const_iterator it = lu.vecinos.begin(); it != lu.vecinos.end(); ++it) {
                const string& v = it->first;
                int w = it->second;
                if (!nivel.count(v)) {
                    nivel[v] = nivel[u] + 1;
                    cola.push(v);
                }
                if (nivel[v] == nivel[u] + 1)
                    afinidad[v] += afinidad[u] * ((double)w / total) * pow(alfa, nivel[v] - 1);
            }
        }

        vector<string> candidatos;
        for (map<string, double>::iterator it = afinidad.begin(); it != afinidad.end(); ++it) {
            const string& v = it->first;
            if (v == origen || yaLeidos.count(v)) continue;
            if (grafo.find(v)->second.copiasDisponibles <= 0) continue;
            candidatos.push_back(v);
        }
        sort(candidatos.begin(), candidatos.end(), [&](const string& a, const string& b) {
            if (afinidad[a] != afinidad[b]) return afinidad[a] > afinidad[b];
            int pa = grafo.find(a)->second.totalPrestamos, pb = grafo.find(b)->second.totalPrestamos;
            if (pa != pb) return pa > pb;
            return a < b;
        });

        const Libro& lo = grafo.find(origen)->second;
        vector<Recomendacion> resultado;
        for (size_t i = 0; i < candidatos.size() && (int)resultado.size() < k; i++) {
            Recomendacion r;
            r.libro = &grafo.find(candidatos[i])->second;
            r.afinidad = afinidad[candidatos[i]];
            int nv = nivel[candidatos[i]];
            if (nv == 1) r.motivo = "Quienes leyeron '" + lo.titulo + "' tambien lo prestaron";
            else {
                ostringstream os;
                os << "Conexion indirecta (nivel " << nv << ")";
                r.motivo = os.str();
            }
            resultado.push_back(r);
        }

        if ((int)resultado.size() < k) {
            set<string> usados = yaLeidos;
            usados.insert(origen);
            for (size_t i = 0; i < resultado.size(); i++) usados.insert(resultado[i].libro->id);
            vector<const Libro*> pop = masPopulares((int)grafo.size());
            for (int pasada = 0; pasada < 2; pasada++) {
                for (size_t i = 0; i < pop.size() && (int)resultado.size() < k; i++) {
                    const Libro* b = pop[i];
                    bool mismaCat = (b->categoria == lo.categoria);
                    if ((pasada == 0) != mismaCat) continue;
                    if (usados.count(b->id) || b->copiasDisponibles <= 0) continue;
                    Recomendacion r;
                    r.libro = b;
                    r.afinidad = 0.0;
                    r.motivo = mismaCat ? "Popular en el mismo tema" : "Popular en la biblioteca";
                    resultado.push_back(r);
                    usados.insert(b->id);
                }
            }
        }
        return resultado;
    }
};

string leer(const string& texto) {
    cout << texto;
    string s;
    if (!getline(cin, s)) {  
        cout << endl;
        exit(0);
    }
    size_t a = s.find_first_not_of(" \t\r\n");
    if (a == string::npos) return "";
    size_t b = s.find_last_not_of(" \t\r\n");
    return s.substr(a, b - a + 1);
}

int leerEntero(const string& texto, int minimo) {
    while (true) {
        string s = leer(texto);
        char* fin = NULL;
        long n = strtol(s.c_str(), &fin, 10);
        if (!s.empty() && *fin == '\0' && n >= minimo) return (int)n;
        cout << "  Valor no valido. Ingrese un numero entero >= " << minimo << "." << endl;
    }
}

string menu() {
    cout << "\n===== BIBLIOTECA INTELIGENTE =====\n"
         << "1. Registrar nuevo libro\n"
         << "2. Registrar prestamo\n"
         << "3. Ver libros mas populares\n"
         << "4. Obtener recomendaciones\n"
         << "5. Consultar conexiones de un libro\n"
         << "6. Registrar devolucion\n"
         << "0. Salir\n";
    return leer("Opcion: ");
}

void opcionRegistrarLibro(Biblioteca& bib) {
    string id = aMayus(leer("ID/ISBN: "));
    if (id.empty() || bib.grafo.count(id)) {
        cout << "  Error: el ID esta vacio o ya existe." << endl;
        return;
    }
    string titulo = leer("Titulo: ");
    string autor = leer("Autor: ");
    string categoria = leer("Categoria/tema: ");
    int paginas = leerEntero("Paginas: ", 1);
    int anio = leerEntero("Ano de publicacion: ", 0);
    int copias = leerEntero("Numero de copias: ", 1);
    bib.agregarLibro(Libro(id, titulo, autor, categoria, paginas, anio, copias));
    cout << "  Libro '" << titulo << "' registrado correctamente." << endl;
}

void opcionRegistrarPrestamo(Biblioteca& bib) {
    string usuario = aMayus(leer("ID de usuario (ej. U001): "));
    string id = aMayus(leer("ID del libro (ej. L01): "));
    if (usuario.empty()) {
        cout << "  Error: debe indicar un usuario." << endl;
        return;
    }
    string msg;
    bib.registrarPrestamo(usuario, id, hoy(), true, msg);
    cout << "  " << msg << endl;
}

void opcionPopulares(Biblioteca& bib) {
    int n = leerEntero("Cuantos libros desea ver? ", 1);
    cout << "\nTop " << n << " por numero de prestamos:" << endl;
    vector<const Libro*> top = bib.masPopulares(n);
    for (size_t i = 0; i < top.size(); i++) {
        char num[16];
        snprintf(num, sizeof(num), " %2d. ", (int)i + 1);
        cout << num << rellenar(top[i]->titulo, 45) << " " << top[i]->totalPrestamos
             << " prestamos  [" << top[i]->categoria << "]" << endl;
    }
}

void opcionRecomendar(Biblioteca& bib) {
    string usuario = aMayus(leer("ID de usuario: "));
    string id = aMayus(leer("ID del libro prestado (Enter = ultimo de su historial): "));
    if (id.empty()) {
        map<string, pair<string, long> >::iterator it = bib.ultimoPrestamo.find(usuario);
        if (it == bib.ultimoPrestamo.end()) {
            cout << "  Error: el usuario no tiene historial; indique un libro." << endl;
            return;
        }
        id = it->second.first;
    }
    if (!bib.grafo.count(id)) {
        cout << "  Error: el libro '" << id << "' no existe." << endl;
        return;
    }
    cout << "\nLibro base: \"" << bib.grafo[id].titulo << "\"" << endl;
    vector<Recomendacion> recs = bib.recomendar(usuario, id);
    if (recs.empty()) {
        cout << "  No hay recomendaciones disponibles." << endl;
        return;
    }
    cout << "Recomendaciones para usted:" << endl;
    for (size_t i = 0; i < recs.size(); i++) {
        char af[32];
        snprintf(af, sizeof(af), "(afinidad %.2f)", recs[i].afinidad);
        cout << " " << i + 1 << ". " << rellenar(recs[i].libro->titulo, 45) << " " << af << endl;
        cout << "      " << recs[i].libro->autor << " - " << recs[i].libro->categoria
             << " | Motivo: " << recs[i].motivo << endl;
    }
}

void opcionConexiones(Biblioteca& bib) {
    string id = aMayus(leer("ID del libro: "));
    map<string, Libro>::iterator it = bib.grafo.find(id);
    if (it == bib.grafo.end()) {
        cout << "  Error: el libro no existe." << endl;
        return;
    }
    Libro& libro = it->second;
    cout << "\nLibro: \"" << libro.titulo << "\"" << endl;
    cout << "Despues de este libro se suele prestar:" << endl;
    if (libro.vecinos.empty()) cout << "  (sin conexiones salientes)" << endl;
    vector<pair<int, string> > sal;
    for (map<string, int>::iterator v = libro.vecinos.begin(); v != libro.vecinos.end(); ++v)
        sal.push_back(make_pair(-v->second, v->first));
    sort(sal.begin(), sal.end());
    for (size_t i = 0; i < sal.size(); i++)
        cout << "  -> " << rellenar(bib.grafo[sal[i].second].titulo, 45) << " peso " << -sal[i].first << endl;

    cout << "Antes de este libro se suele prestar:" << endl;
    vector<pair<int, string> > ent;
    for (map<string, Libro>::iterator b = bib.grafo.begin(); b != bib.grafo.end(); ++b) {
        map<string, int>::iterator e = b->second.vecinos.find(id);
        if (e != b->second.vecinos.end()) ent.push_back(make_pair(-e->second, b->first));
    }
    if (ent.empty()) cout << "  (sin conexiones entrantes)" << endl;
    sort(ent.begin(), ent.end());
    for (size_t i = 0; i < ent.size(); i++)
        cout << "  <- " << rellenar(bib.grafo[ent[i].second].titulo, 45) << " peso " << -ent[i].first << endl;
}

void opcionDevolucion(Biblioteca& bib) {
    string usuario = aMayus(leer("ID de usuario: "));
    string id = aMayus(leer("ID del libro: "));
    string msg;
    bib.registrarDevolucion(usuario, id, msg);
    cout << "  " << msg << endl;
}

int main() {
    Biblioteca bib;
    if (!bib.cargarLibros(ARCHIVO_LIBROS)) {
        cout << "No se encontro '" << ARCHIVO_LIBROS << "'. Subalo al proyecto." << endl;
        return 1;
    }
    {
        ifstream prueba(ARCHIVO_PRESTAMOS.c_str());
        if (!prueba) bib.generarPrestamosSimulados(ARCHIVO_PRESTAMOS);
    }
    bib.cargarPrestamos(ARCHIVO_PRESTAMOS);

    int nPrestamos = 0, nAristas = 0;
    for (map<string, Libro>::iterator it = bib.grafo.begin(); it != bib.grafo.end(); ++it) {
        nPrestamos += it->second.totalPrestamos;
        nAristas += (int)it->second.vecinos.size();
    }
    cout << "Datos cargados: " << bib.grafo.size() << " libros, " << nPrestamos
         << " prestamos, " << nAristas << " conexiones, " << bib.historial.size()
         << " usuarios." << endl;

    while (true) {
        string op = menu();
        if (op == "0") { cout << "¡Hasta pronto!" << endl; break; }
        else if (op == "1") opcionRegistrarLibro(bib);
        else if (op == "2") opcionRegistrarPrestamo(bib);
        else if (op == "3") opcionPopulares(bib);
        else if (op == "4") opcionRecomendar(bib);
        else if (op == "5") opcionConexiones(bib);
        else if (op == "6") opcionDevolucion(bib);
        else cout << "  Opcion no valida." << endl;
    }
    return 0;
}