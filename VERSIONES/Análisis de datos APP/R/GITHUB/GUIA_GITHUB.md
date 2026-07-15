# Guía para subir Bonifaz-INCMNSZ a GitHub

Avanzamos de un paso al siguiente solo cuando digas "listo".

## Antes de empezar
- [x] Git instalado (git version 2.52.0.windows.1)
- [ ] Cuenta de GitHub creada

## Paso 1 — Preparar la carpeta local del proyecto
Reúne en UNA sola carpeta (por ejemplo `Bonifaz-INCMNSZ`) los archivos que
vas a subir:
- `app.R`
- `README.md`
- `.gitignore`   (incluido en esta entrega)

## Paso 2 — Crear el repositorio en GitHub (vacío)
En github.com: botón "New" → nombre del repositorio (ej. `Bonifaz-INCMNSZ`)
→ NO marques "Add a README" (ya tenemos uno) → "Create repository".

## Paso 3 — Inicializar Git en tu carpeta local
En PowerShell, dentro de la carpeta del proyecto:
```powershell
cd "C:\ruta\a\Bonifaz-INCMNSZ"
git init
git add .
git commit -m "Primera versión de Bonifaz-INCMNSZ"
```

## Paso 4 — Conectar tu carpeta con el repositorio de GitHub
```powershell
git branch -M main
git remote add origin https://github.com/TU_USUARIO/Bonifaz-INCMNSZ.git
```
(Reemplaza `TU_USUARIO` por tu nombre de usuario de GitHub.)

## Paso 5 — Subir los archivos
```powershell
git push -u origin main
```
La primera vez, Git te pedirá iniciar sesión en GitHub (se abre una ventana
del navegador para autorizar).

## Paso 6 — Verificar
Recarga la página de tu repositorio en GitHub: deberían aparecer `app.R`,
`README.md` y verse el contenido del README en la portada.

## Más adelante (cuando cambies la app)
Para subir nuevas versiones:
```powershell
git add .
git commit -m "Descripción del cambio"
git push
```
