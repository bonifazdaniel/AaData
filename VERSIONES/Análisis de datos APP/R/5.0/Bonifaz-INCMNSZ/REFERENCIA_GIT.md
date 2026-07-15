# Referencia rápida de Git — Bonifaz-INCMNSZ

Tu repositorio: https://github.com/bonifazdaniel/Bonifaz-INCMNSZ

La configuración inicial ya quedó lista (una sola vez). De aquí en adelante,
solo necesitas los 3 comandos de "subir cambios".

---

## Cada vez que cambies la app y quieras subirla

Abre PowerShell dentro de la carpeta del proyecto y ejecuta, uno por uno:

Paso 1 — párate en la carpeta (si no estás ya):
```powershell
cd "C:\Bonifaz-INCMNSZ"
```

Paso 2 — prepara los cambios:
```powershell
git add .
```

Paso 3 — guarda los cambios con un mensaje que describa qué hiciste:
```powershell
git commit -m "Describe aqui tu cambio"
```
(Ejemplos de mensaje: "Corregi tabla de frecuencias", "Agregue nueva grafica".)

Paso 4 — súbelos a GitHub:
```powershell
git push
```

Listo. Recarga la página del repositorio (F5) para ver los cambios.

---

## Comandos útiles

Ver qué archivos cambiaron (antes de subir):
```powershell
git status
```

Ver el historial de versiones subidas:
```powershell
git log --oneline
```

Traer a tu computadora cambios hechos desde otro lugar / la web:
```powershell
git pull
```

---

## Recordatorios

- El archivo `.gitignore` evita subir bases de datos (`.csv`, `.xlsx`, `.xls`)
  por seguridad de los datos de pacientes. Si algún día quieres subir un
  archivo de ejemplo, avísame y ajustamos la regla.
- Si al hacer `commit` se abre el editor Vim, sal con: `Esc`, luego `:q!` y
  Enter; y repite el commit usando `-m "tu mensaje"`.
- Si `git push` vuelve a pedir autenticación, usa la ventana del navegador
  ("Sign in with your browser") o el token personal.
