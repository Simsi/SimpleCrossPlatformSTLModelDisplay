using System;
using System.Buffers.Binary;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.ComponentModel;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Numerics;
using System.Threading;
using System.Threading.Tasks;
using Avalonia;
using Avalonia.Controls;
using Avalonia.Input;
using Avalonia.Media;

namespace TestGPT5.Classes
{
    public class StlView3D : Control
    {
        #region Styled properties (камера/отрисовка)
        public static readonly StyledProperty<double> RotationXProperty =
            AvaloniaProperty.Register<StlView3D, double>(nameof(RotationX), 0d);

        public static readonly StyledProperty<double> RotationYProperty =
            AvaloniaProperty.Register<StlView3D, double>(nameof(RotationY), 0d);

        public static readonly StyledProperty<double> ZoomProperty =
            AvaloniaProperty.Register<StlView3D, double>(nameof(Zoom), 1.0, coerce: (_, v) => Math.Clamp(v, 0.2, 8.0));

        public static readonly StyledProperty<bool> AutoScaleToFitProperty =
            AvaloniaProperty.Register<StlView3D, bool>(nameof(AutoScaleToFit), true);

        public static readonly StyledProperty<bool> IsPerspectiveProperty =
            AvaloniaProperty.Register<StlView3D, bool>(nameof(IsPerspective), true);

        public static readonly StyledProperty<bool> WireframeProperty =
            AvaloniaProperty.Register<StlView3D, bool>(nameof(Wireframe), false);

        public static readonly StyledProperty<Color> SceneBackgroundProperty =
            AvaloniaProperty.Register<StlView3D, Color>(nameof(SceneBackground), Color.FromRgb(16, 16, 20));

        public double RotationX { get => GetValue(RotationXProperty); set => SetValue(RotationXProperty, value); }
        public double RotationY { get => GetValue(RotationYProperty); set => SetValue(RotationYProperty, value); }
        public double Zoom { get => GetValue(ZoomProperty); set => SetValue(ZoomProperty, value); }
        public bool AutoScaleToFit { get => GetValue(AutoScaleToFitProperty); set => SetValue(AutoScaleToFitProperty, value); }
        public bool IsPerspective { get => GetValue(IsPerspectiveProperty); set => SetValue(IsPerspectiveProperty, value); }
        public bool Wireframe { get => GetValue(WireframeProperty); set => SetValue(WireframeProperty, value); }
        public Color SceneBackground { get => GetValue(SceneBackgroundProperty); set => SetValue(SceneBackgroundProperty, value); }
        #endregion

        #region Styled properties (освещение/“шейдер”)
        public static readonly StyledProperty<bool> LightingEnabledProperty =
            AvaloniaProperty.Register<StlView3D, bool>(nameof(LightingEnabled), true);
        public bool LightingEnabled { get => GetValue(LightingEnabledProperty); set => SetValue(LightingEnabledProperty, value); }

        public enum ShadingModel { Unlit = 0, Lambert = 1, BlinnPhong = 2 }
        public static readonly StyledProperty<ShadingModel> ShadingProperty =
            AvaloniaProperty.Register<StlView3D, ShadingModel>(nameof(Shading), ShadingModel.BlinnPhong);
        public ShadingModel Shading { get => GetValue(ShadingProperty); set => SetValue(ShadingProperty, value); }

        public static readonly StyledProperty<bool> UsePointLightProperty =
            AvaloniaProperty.Register<StlView3D, bool>(nameof(UsePointLight), false);
        public bool UsePointLight { get => GetValue(UsePointLightProperty); set => SetValue(UsePointLightProperty, value); }

        public static readonly StyledProperty<double> AmbientProperty =
            AvaloniaProperty.Register<StlView3D, double>(nameof(Ambient), 0.15, coerce: (_, v) => Math.Clamp(v, 0, 1));
        public static readonly StyledProperty<double> DiffuseProperty =
            AvaloniaProperty.Register<StlView3D, double>(nameof(Diffuse), 0.85, coerce: (_, v) => Math.Clamp(v, 0, 1));
        public static readonly StyledProperty<double> SpecularProperty =
            AvaloniaProperty.Register<StlView3D, double>(nameof(Specular), 0.25, coerce: (_, v) => Math.Clamp(v, 0, 1));
        public static readonly StyledProperty<double> ShininessProperty =
            AvaloniaProperty.Register<StlView3D, double>(nameof(Shininess), 32.0, coerce: (_, v) => Math.Clamp(v, 1, 256));

        public double Ambient { get => GetValue(AmbientProperty); set => SetValue(AmbientProperty, value); }
        public double Diffuse { get => GetValue(DiffuseProperty); set => SetValue(DiffuseProperty, value); }
        public double Specular { get => GetValue(SpecularProperty); set => SetValue(SpecularProperty, value); }
        public double Shininess { get => GetValue(ShininessProperty); set => SetValue(ShininessProperty, value); }

        public static readonly StyledProperty<double> LightYawProperty =
            AvaloniaProperty.Register<StlView3D, double>(nameof(LightYaw), -45.0);
        public static readonly StyledProperty<double> LightPitchProperty =
            AvaloniaProperty.Register<StlView3D, double>(nameof(LightPitch), -35.0);
        public static readonly StyledProperty<double> LightDistanceProperty =
            AvaloniaProperty.Register<StlView3D, double>(nameof(LightDistance), 6.0, coerce: (_, v) => Math.Clamp(v, 0.1, 100));
        public static readonly StyledProperty<double> LightAttenuationProperty =
            AvaloniaProperty.Register<StlView3D, double>(nameof(LightAttenuation), 0.03, coerce: (_, v) => Math.Clamp(v, 0.0, 1.0));
        public static readonly StyledProperty<Color> LightColorProperty =
            AvaloniaProperty.Register<StlView3D, Color>(nameof(LightColor), Colors.White);

        public double LightYaw { get => GetValue(LightYawProperty); set => SetValue(LightYawProperty, value); }
        public double LightPitch { get => GetValue(LightPitchProperty); set => SetValue(LightPitchProperty, value); }
        public double LightDistance { get => GetValue(LightDistanceProperty); set => SetValue(LightDistanceProperty, value); }
        public double LightAttenuation { get => GetValue(LightAttenuationProperty); set => SetValue(LightAttenuationProperty, value); }
        public Color LightColor { get => GetValue(LightColorProperty); set => SetValue(LightColorProperty, value); }

        public static readonly StyledProperty<bool> BackfaceCullingProperty =
            AvaloniaProperty.Register<StlView3D, bool>(nameof(BackfaceCulling), true);
        public static readonly StyledProperty<bool> TwoSidedLightingProperty =
            AvaloniaProperty.Register<StlView3D, bool>(nameof(TwoSidedLighting), false);

        public bool BackfaceCulling { get => GetValue(BackfaceCullingProperty); set => SetValue(BackfaceCullingProperty, value); }
        public bool TwoSidedLighting { get => GetValue(TwoSidedLightingProperty); set => SetValue(TwoSidedLightingProperty, value); }
        #endregion

        #region Readonly/debug bindables
        public static readonly StyledProperty<string> CursorInfoProperty =
            AvaloniaProperty.Register<StlView3D, string>(nameof(CursorInfo), "x:— y:— z:—");
        public string CursorInfo { get => GetValue(CursorInfoProperty); private set => SetValue(CursorInfoProperty, value); }

        public static readonly StyledProperty<string> HoverInfoProperty =
            AvaloniaProperty.Register<StlView3D, string>(nameof(HoverInfo), "Hover: —");
        public string HoverInfo { get => GetValue(HoverInfoProperty); private set => SetValue(HoverInfoProperty, value); }
        #endregion

        #region Scene data
        internal struct Triangle { public Vector3 A, B, C; }

        public class SceneObjectInfo : INotifyPropertyChanged
        {
            public event PropertyChangedEventHandler? PropertyChanged;
            void OnChanged(string n) => PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(n));

            public string Name { get => _name; set { _name = value; OnChanged(nameof(Name)); } }
            public Color Color { get => _color; set { _color = value; OnChanged(nameof(Color)); } }
            public bool Visible { get => _visible; set { _visible = value; OnChanged(nameof(Visible)); } }
            public Matrix4x4 Transform { get => _transform; set { _transform = value; OnChanged(nameof(Transform)); } }
            public Vector3 Min { get; internal set; }
            public Vector3 Max { get; internal set; }
            public int TriangleCount => Triangles.Count;

            internal List<Triangle> Triangles = new();
            internal string _name = "Object";
            internal Color _color = Color.FromRgb(127, 178, 255);
            internal bool _visible = true;
            internal Matrix4x4 _transform = Matrix4x4.Identity;
        }

        public ObservableCollection<SceneObjectInfo> SceneObjects { get; } = new();

        // Глобальные границы — по всем видимым объектам
        private Vector3 _min = new(float.PositiveInfinity);
        private Vector3 _max = new(float.NegativeInfinity);

        private bool _hasAnything;
        #endregion

        #region Ctor & events
        public StlView3D()
        {
            Focusable = true;
            Cursor = new Cursor(StandardCursorType.Arrow);

            void Redraw<T>(StyledProperty<T> p) => this.GetObservable(p).Subscribe(_ => InvalidateVisual());

            Redraw(RotationXProperty); Redraw(RotationYProperty);
            Redraw(ZoomProperty); Redraw(AutoScaleToFitProperty);
            Redraw(IsPerspectiveProperty); Redraw(WireframeProperty);
            Redraw(SceneBackgroundProperty);

            Redraw(LightingEnabledProperty); Redraw(ShadingProperty);
            Redraw(UsePointLightProperty); Redraw(AmbientProperty);
            Redraw(DiffuseProperty); Redraw(SpecularProperty);
            Redraw(ShininessProperty); Redraw(LightYawProperty);
            Redraw(LightPitchProperty); Redraw(LightDistanceProperty);
            Redraw(LightAttenuationProperty); Redraw(LightColorProperty);
            Redraw(BackfaceCullingProperty); Redraw(TwoSidedLightingProperty);

            PointerPressed += (_, __) => Focus();
            KeyDown += OnKeyDown;
            PointerWheelChanged += OnWheel;
            PointerMoved += OnPointerMoved;

            AddHandler(DragDrop.DragOverEvent, OnDragOver);
            AddHandler(DragDrop.DropEvent, OnDrop);
            DragDrop.SetAllowDrop(this, true);
        }
        #endregion

        #region Public API
        public void ClearScene()
        {
            SceneObjects.Clear();
            _min = new Vector3(float.PositiveInfinity);
            _max = new Vector3(float.NegativeInfinity);
            _hasAnything = false;
            CursorInfo = "x:— y:— z:—";
            HoverInfo = "Hover: —";
            InvalidateVisual();
        }

        public async Task LoadFromStreamAsync(Stream stream, CancellationToken ct = default)
        {
            using var ms = new MemoryStream();
            await stream.CopyToAsync(ms, 81920, ct);
            var data = ms.ToArray();
            var tris = ParseStl(data);
            AddMeshAsObject(tris, "STL Model");
        }

        public void LoadFromStream(Stream stream)
        {
            using var ms = new MemoryStream();
            stream.CopyTo(ms);
            var data = ms.ToArray();
            var tris = ParseStl(data);
            AddMeshAsObject(tris, "STL Model");
        }

        public void AddPrimitive(string kind)
        {
            kind = kind?.ToLowerInvariant() ?? "";
            List<(Vector3 A, Vector3 B, Vector3 C)> tris = kind switch
            {
                "cube" => CreateCube(1f),
                "sphere" => CreateUvSphere(0.5f, 24, 16),
                "cylinder" => CreateCylinder(0.5f, 1.0f, 32),
                _ => CreateCube(1f),
            };
            AddMeshAsObject(tris, $"{char.ToUpper(kind[0])}{kind[1..]} {SceneObjects.Count(o => o.Name.StartsWith(char.ToUpper(kind[0]) + kind[1..])) + 1}");
        }

        public SceneObjectInfo AddMeshAsObject(IEnumerable<(Vector3 A, Vector3 B, Vector3 C)> triangles, string name, Color? color = null)
        {
            var obj = new SceneObjectInfo { Name = name, Color = color ?? Color.FromRgb(127, 178, 255) };
            obj.Triangles = triangles.Select(t => new Triangle { A = t.A, B = t.B, C = t.C }).ToList();
            if (obj.Triangles.Count == 0) return obj;

            var min = new Vector3(float.PositiveInfinity);
            var max = new Vector3(float.NegativeInfinity);
            foreach (var tr in obj.Triangles)
            {
                min = Vector3.Min(min, Vector3.Min(tr.A, Vector3.Min(tr.B, tr.C)));
                max = Vector3.Max(max, Vector3.Max(tr.A, Vector3.Max(tr.B, tr.C)));
            }
            obj.Min = min; obj.Max = max;

            SceneObjects.Add(obj);
            RecalcSceneBounds();
            _hasAnything = SceneObjects.Count > 0;
            InvalidateVisual();
            return obj;
        }
        #endregion

        #region Input handlers
        private void OnKeyDown(object? sender, KeyEventArgs e)
        {
            const double step = 5.0;
            switch (e.Key)
            {
                case Key.Left: RotationY -= step; e.Handled = true; break;
                case Key.Right: RotationY += step; e.Handled = true; break;
                case Key.Up: RotationX -= step; e.Handled = true; break;
                case Key.Down: RotationX += step; e.Handled = true; break;
                case Key.R: RotationX = RotationY = 0; Zoom = 1; e.Handled = true; break;
            }
        }

        private void OnWheel(object? sender, PointerWheelEventArgs e)
        {
            // e.Delta.Y > 0 => вверх (приближение)
            double factor = Math.Pow(1.12, e.Delta.Y);
            Zoom = Math.Clamp(Zoom * factor, 0.2, 8.0);
            e.Handled = true;
        }

        private void OnPointerMoved(object? sender, PointerEventArgs e)
        {
            var p = e.GetPosition(this);
            UpdateCursorAndHover((float)p.X, (float)p.Y);
        }

        private void OnDragOver(object? sender, DragEventArgs e)
        {
            var kind = e.Data.GetText()?.ToLowerInvariant();
            if (kind is "cube" or "sphere" or "cylinder")
                e.DragEffects = DragDropEffects.Copy;
            else
                e.DragEffects = DragDropEffects.None;

            e.Handled = true;
        }

        private void OnDrop(object? sender, DragEventArgs e)
        {
            var kind = e.Data.GetText()?.ToLowerInvariant();
            if (string.IsNullOrWhiteSpace(kind)) return;

            var p = e.GetPosition(this);

            if (TryMakeRay((float)p.X, (float)p.Y, out var ro, out var rd) &&
                Math.Abs(rd.Z) > 1e-6f)
            {
                // пересечение луча с плоскостью z=0
                float t = (0 - ro.Z) / rd.Z;
                var hit = ro + t * rd; // мировые координаты

                AddPrimitive(kind, hit); // <-- кладём в точку
                e.Handled = true;
            }
        }

        private bool TryMakeRay(float sx, float sy, out Vector3 origin, out Vector3 dir)
        {
            origin = default; dir = default;
            if (Bounds.Width <= 1 || Bounds.Height <= 1) return false;

            var rect = new Rect(Bounds.Size);
            double cx = rect.Width / 2.0, cy = rect.Height / 2.0;
            double nx = sx - cx, ny = sy - cy;

            float sceneExtent = (_max - _min).Length();
            float baseDist = Math.Max(3f * sceneExtent, 1f);
            float cameraZ = IsPerspective ? (float)(baseDist / Zoom) : baseDist;

            if (IsPerspective)
            {
                origin = new Vector3(0, 0, -cameraZ);
                dir = Vector3.Normalize(new Vector3((float)nx, (float)-ny, (float)cameraZ));
            }
            else
            {
                float invZoom = (float)(1.0 / Zoom);
                origin = new Vector3((float)nx * invZoom, (float)-ny * invZoom, -cameraZ);
                dir = Vector3.UnitZ;
            }
            return true;
        }

        public SceneObjectInfo AddPrimitive(string kind, Vector3? position = null)
        {
            kind = kind?.ToLowerInvariant() ?? "";
            List<(Vector3 A, Vector3 B, Vector3 C)> tris = kind switch
            {
                "cube" => CreateCube(1f),
                "sphere" => CreateUvSphere(0.5f, 24, 16),
                "cylinder" => CreateCylinder(0.5f, 1.0f, 32),
                _ => CreateCube(1f),
            };

            var obj = AddMeshAsObject(tris, $"{char.ToUpper(kind[0])}{kind[1..]} {SceneObjects.Count(o => o.Name.StartsWith(char.ToUpper(kind[0]) + kind[1..])) + 1}");
            if (position.HasValue)
                obj.Transform = Matrix4x4.CreateTranslation(position.Value);
            InvalidateVisual();
            return obj;
        }

        

        #endregion

        #region Render
        protected override Size MeasureOverride(Size availableSize)
            => new(Math.Clamp(availableSize.Width, 100, double.IsInfinity(availableSize.Width) ? 600 : availableSize.Width),
                   Math.Clamp(availableSize.Height, 100, double.IsInfinity(availableSize.Height) ? 400 : availableSize.Height));

        public override void Render(DrawingContext context)
        {
            base.Render(context);

            var rect = new Rect(Bounds.Size);
            context.FillRectangle(new SolidColorBrush(SceneBackground), rect);

            if (!_hasAnything || rect.Width <= 1 || rect.Height <= 1)
                return;

            // Повороты сцены (все объекты в одном мире)
            float rx = (float)(RotationX * Math.PI / 180.0);
            float ry = (float)(RotationY * Math.PI / 180.0);
            var worldRot = Matrix4x4.CreateRotationY(ry) * Matrix4x4.CreateRotationX(rx);

            // Камера
            float sceneExtent = (_max - _min).Length();
            float baseDist = Math.Max(3f * sceneExtent, 1f);
            float cameraZ = IsPerspective ? (float)(baseDist / Zoom) : baseDist; // орто не зависит от Zoom, см. масштаб ниже

            var (lightDir, lightPos) = ComputeLightVectors();

            // Pre-pass: собрать проекции
            var prepared = new List<PreparedTriangle>(capacity: 1024);
            double minX = double.PositiveInfinity, minY = double.PositiveInfinity;
            double maxX = double.NegativeInfinity, maxY = double.NegativeInfinity;

            foreach (var obj in SceneObjects)
            {
                if (!obj.Visible || obj.Triangles.Count == 0) continue;

                var center = (obj.Min + obj.Max) * 0.5f;
                var toOrigin = Matrix4x4.CreateTranslation(-center);
                var objWorld = obj.Transform * toOrigin * worldRot; // сначала локальная трансформация, потом общий поворот

                foreach (var t in obj.Triangles)
                {
                    var a = Vector3.Transform(t.A, objWorld);
                    var b = Vector3.Transform(t.B, objWorld);
                    var c = Vector3.Transform(t.C, objWorld);

                    var n = Vector3.Normalize(Vector3.Cross(b - a, c - a));
                    var p = (a + b + c) / 3f;

                    var viewPos = new Vector3(0, 0, -cameraZ);
                    var V = Vector3.Normalize(viewPos - p);

                    float ndotl, spec, atten = 1f;

                    if (!LightingEnabled || Shading == ShadingModel.Unlit)
                    {
                        ndotl = 1f; spec = 0f;
                    }
                    else
                    {
                        Vector3 L;
                        if (UsePointLight)
                        {
                            L = Vector3.Normalize(lightPos - p);
                            float d = Math.Max(0.0001f, (lightPos - p).Length());
                            atten = 1f / (1f + (float)(LightAttenuation * d * d));
                        }
                        else
                        {
                            L = Vector3.Normalize(-lightDir);
                        }

                        ndotl = Vector3.Dot(n, L);
                        if (TwoSidedLighting) ndotl = MathF.Abs(ndotl);
                        ndotl = Math.Max(0f, ndotl);

                        if (BackfaceCulling)
                        {
                            var facing = Vector3.Dot(n, V);
                            if (facing <= 0f) continue;
                        }

                        if (Shading == ShadingModel.BlinnPhong && Specular > 0)
                        {
                            var H = Vector3.Normalize(L + V);
                            spec = MathF.Pow(Math.Max(0f, Vector3.Dot(n, H)), (float)Shininess);
                        }
                        else spec = 0f;
                    }

                    Vector2 pa, pb, pc;
                    float za, zb, zc;

                    if (IsPerspective)
                    {
                        za = a.Z + cameraZ; zb = b.Z + cameraZ; zc = c.Z + cameraZ;
                        za = Math.Max(0.01f, za); zb = Math.Max(0.01f, zb); zc = Math.Max(0.01f, zc);
                        float f = cameraZ;
                        pa = new Vector2(a.X * (f / za), -a.Y * (f / za));
                        pb = new Vector2(b.X * (f / zb), -b.Y * (f / zb));
                        pc = new Vector2(c.X * (f / zc), -c.Y * (f / zc));
                    }
                    else
                    {
                        za = a.Z; zb = b.Z; zc = c.Z;
                        float zFactor = (float)Zoom; // орто масштабируем “камеру” через Zoom
                        pa = new Vector2(a.X * zFactor, -a.Y * zFactor);
                        pb = new Vector2(b.X * zFactor, -b.Y * zFactor);
                        pc = new Vector2(c.X * zFactor, -c.Y * zFactor);
                    }

                    var depth = (za + zb + zc) / 3f;
                    var colorKey = ComputeFaceColorKey(obj.Color, LightColor,
                        (float)Ambient, (float)Diffuse, (float)Specular, ndotl, spec, atten,
                        LightingEnabled, Shading);

                    prepared.Add(new PreparedTriangle(pa, pb, pc, depth, colorKey));

                    UpdateBounds(pa, ref minX, ref minY, ref maxX, ref maxY);
                    UpdateBounds(pb, ref minX, ref minY, ref maxX, ref maxY);
                    UpdateBounds(pc, ref minX, ref minY, ref maxX, ref maxY);
                }
            }

            if (prepared.Count == 0) return;

            double width = maxX - minX;
            double height = maxY - minY;
            if (width <= 0 || height <= 0) return;

            double margin = 12;
            double sx = (rect.Width - 2 * margin) / width;
            double sy = (rect.Height - 2 * margin) / height;
            double scale = Math.Min(sx, sy);

            double cx = rect.Width / 2.0;
            double cy = rect.Height / 2.0;
            double ox = (minX + maxX) / 2.0;
            double oy = (minY + maxY) / 2.0;

            prepared.Sort((l, r) => r.Depth.CompareTo(l.Depth));

            var brushCache = new Dictionary<int, IBrush>(256);
            var pen = new Pen(new SolidColorBrush(Colors.White), 1, lineCap: PenLineCap.Round, lineJoin: PenLineJoin.Round);

            foreach (var ptri in prepared)
            {
                var p0 = ScaleToView(ptri.P0, scale, cx, cy, ox, oy);
                var p1 = ScaleToView(ptri.P1, scale, cx, cy, ox, oy);
                var p2 = ScaleToView(ptri.P2, scale, cx, cy, ox, oy);

                var geom = new StreamGeometry();
                using (var gc = geom.Open())
                {
                    gc.BeginFigure(p0, isFilled: true);
                    gc.LineTo(p1);
                    gc.LineTo(p2);
                    gc.EndFigure(isClosed: true);
                }

                if (!Wireframe)
                {
                    if (!brushCache.TryGetValue(ptri.ColorKey, out var brush))
                    {
                        byte r = (byte)((ptri.ColorKey >> 16) & 0xFF);
                        byte g = (byte)((ptri.ColorKey >> 8) & 0xFF);
                        byte b = (byte)(ptri.ColorKey & 0xFF);
                        brush = new SolidColorBrush(Color.FromRgb(r, g, b));
                        brushCache[ptri.ColorKey] = brush;
                    }
                    context.DrawGeometry(brush, null, geom);
                }
                else
                {
                    context.DrawGeometry(null, pen, geom);
                }
            }
        }

        private readonly struct PreparedTriangle
        {
            public readonly Vector2 P0, P1, P2;
            public readonly float Depth;
            public readonly int ColorKey;
            public PreparedTriangle(Vector2 p0, Vector2 p1, Vector2 p2, float depth, int colorKey)
            { P0 = p0; P1 = p1; P2 = p2; Depth = depth; ColorKey = colorKey; }
        }

        private static void UpdateBounds(Vector2 p, ref double minX, ref double minY, ref double maxX, ref double maxY)
        {
            if (p.X < minX) minX = p.X;
            if (p.Y < minY) minY = p.Y;
            if (p.X > maxX) maxX = p.X;
            if (p.Y > maxY) maxY = p.Y;
        }

        private static Point ScaleToView(Vector2 p, double scale, double cx, double cy, double ox, double oy)
            => new((p.X - ox) * scale + cx, (p.Y - oy) * scale + cy);
        #endregion

        #region Picking / cursor
        private void UpdateCursorAndHover(float sx, float sy)
        {
            if (!_hasAnything || Bounds.Width <= 1 || Bounds.Height <= 1)
            {
                CursorInfo = "x:— y:— z:—";
                HoverInfo = "Hover: —";
                return;
            }

            // Инвертируем 2D нормализацию (как в Render) до “проекционной” плоскости
            var rect = new Rect(Bounds.Size);
            double cx = rect.Width / 2.0, cy = rect.Height / 2.0;

            // Повороты сцены
            float rx = (float)(RotationX * Math.PI / 180.0);
            float ry = (float)(RotationY * Math.PI / 180.0);
            var worldRot = Matrix4x4.CreateRotationY(ry) * Matrix4x4.CreateRotationX(rx);

            float sceneExtent = (_max - _min).Length();
            float baseDist = Math.Max(3f * sceneExtent, 1f);
            float cameraZ = IsPerspective ? (float)(baseDist / Zoom) : baseDist;

            // Чтобы восстановить координаты на проекционной плоскости, нам нужен текущий scale и “проекционные” границы.
            // Упростим: считаем ox=oy=0 и scale = 2/shorterSide; вместо этого для луча достаточно нормализовать вектор.
            // Построим луч из камеры через точку экрана.
            // Проекция в Render: perspective: (x * f/z, -y * f/z). Здесь берём “вирт. экран” в z=0 размером по пикселю.
            // Нормализуем экранные координаты к центру:
            double nx = sx - cx;
            double ny = sy - cy;

            Vector3 rayOrigin = new(0, 0, -cameraZ);
            Vector3 rayDir;

            if (IsPerspective)
            {
                // f = cameraZ
                rayDir = Vector3.Normalize(new Vector3((float)nx, (float)-ny, (float)cameraZ));
            }
            else
            {
                // Орто: луч вдоль +Z, origin зависит от nx,ny/Zoom
                float invZoom = (float)(1.0 / Zoom);
                rayOrigin = new((float)nx * invZoom, (float)-ny * invZoom, -cameraZ);
                rayDir = Vector3.UnitZ;
            }

            // Пересечение с плоскостью z=0 (в мировых координатах “после” ротейта)
            // Но мы вращаем объекты, а не камеру. Для курсора покажем просто точку пересечения луча с z=0 в текущем мире.
            if (Math.Abs(rayDir.Z) > 1e-6)
            {
                float t = (0 - rayOrigin.Z) / rayDir.Z;
                var hit = rayOrigin + t * rayDir;
                CursorInfo = $"x:{hit.X:F2}  y:{hit.Y:F2}  z:{hit.Z:F2}";
            }
            else CursorInfo = "x:— y:— z:—";

            // Поиск объекта под курсором (ближайший треугольник)
            float nearestT = float.PositiveInfinity;
            string? bestName = null;

            foreach (var obj in SceneObjects)
            {
                if (!obj.Visible || obj.Triangles.Count == 0) continue;

                var center = (obj.Min + obj.Max) * 0.5f;
                var objWorld = obj.Transform * Matrix4x4.CreateTranslation(-center) * worldRot;

                foreach (var tri in obj.Triangles)
                {
                    var a = Vector3.Transform(tri.A, objWorld);
                    var b = Vector3.Transform(tri.B, objWorld);
                    var c = Vector3.Transform(tri.C, objWorld);

                    if (RayTriangle(rayOrigin, rayDir, a, b, c, out float t))
                    {
                        if (t > 0 && t < nearestT)
                        {
                            nearestT = t;
                            bestName = obj.Name;
                        }
                    }
                }
            }

            HoverInfo = bestName is null ? "Hover: —" : $"Hover: {bestName}";
        }

        private static bool RayTriangle(in Vector3 ro, in Vector3 rd, in Vector3 v0, in Vector3 v1, in Vector3 v2, out float t)
        {
            // Möller–Trumbore
            const float EPS = 1e-7f;
            t = 0;
            var e1 = v1 - v0;
            var e2 = v2 - v0;
            var p = Vector3.Cross(rd, e2);
            float det = Vector3.Dot(e1, p);
            if (det > -EPS && det < EPS) return false;
            float invDet = 1f / det;
            var tv = ro - v0;
            float u = Vector3.Dot(tv, p) * invDet;
            if (u < 0 || u > 1) return false;
            var q = Vector3.Cross(tv, e1);
            float v = Vector3.Dot(rd, q) * invDet;
            if (v < 0 || u + v > 1) return false;
            t = Vector3.Dot(e2, q) * invDet;
            return t > EPS;
        }
        #endregion

        #region Lighting helpers
        private static int ComputeFaceColorKey(Color baseColor, Color lightColor,
                                               float ambient, float diffuse, float specular,
                                               float ndotl, float specAmount, float attenuation,
                                               bool lightingEnabled, ShadingModel shading)
        {
            float br = baseColor.R / 255f, bg = baseColor.G / 255f, bb = baseColor.B / 255f;
            float lr = lightColor.R / 255f, lg = lightColor.G / 255f, lb = lightColor.B / 255f;

            float r, g, b;
            if (!lightingEnabled || shading == ShadingModel.Unlit)
            { r = br; g = bg; b = bb; }
            else
            {
                float diff = diffuse * ndotl * attenuation;
                float spec = (shading == ShadingModel.BlinnPhong) ? (specular * specAmount * attenuation) : 0f;
                r = br * (ambient + diff) + lr * spec;
                g = bg * (ambient + diff) + lg * spec;
                b = bb * (ambient + diff) + lb * spec;
            }

            byte R = (byte)Math.Clamp((int)Math.Round(r * 255f), 0, 255);
            byte G = (byte)Math.Clamp((int)Math.Round(g * 255f), 0, 255);
            byte B = (byte)Math.Clamp((int)Math.Round(b * 255f), 0, 255);
            return (R << 16) | (G << 8) | B;
        }

        private (Vector3 lightDir, Vector3 lightPos) ComputeLightVectors()
        {
            double yaw = LightYaw * Math.PI / 180.0;
            double pitch = LightPitch * Math.PI / 180.0;

            var dir = new Vector3(
                (float)(Math.Cos(pitch) * Math.Cos(yaw)),
                (float)(Math.Sin(pitch)),
                (float)(Math.Cos(pitch) * Math.Sin(yaw)));
            if (dir.LengthSquared() < 1e-6f) dir = new Vector3(0, -1, 0);
            dir = Vector3.Normalize(dir);

            var pos = dir * (float)LightDistance;
            return (dir, pos);
        }
        #endregion

        #region STL + primitives
        private static List<(Vector3 A, Vector3 B, Vector3 C)> ParseStl(byte[] data)
        {
            if (data.Length >= 84)
            {
                uint triCount = BinaryPrimitives.ReadUInt32LittleEndian(data.AsSpan(80, 4));
                long expected = 84L + 50L * triCount;
                if (expected == data.Length) return ParseBinary(data);
            }
            return ParseAscii(data);
        }

        private static List<(Vector3 A, Vector3 B, Vector3 C)> ParseBinary(byte[] data)
        {
            var list = new List<(Vector3, Vector3, Vector3)>();
            uint triCount = BinaryPrimitives.ReadUInt32LittleEndian(data.AsSpan(80, 4));
            int offset = 84;
            for (uint i = 0; i < triCount; i++)
            {
                var a = ReadVec(data, offset + 12);
                var b = ReadVec(data, offset + 24);
                var c = ReadVec(data, offset + 36);
                list.Add((a, b, c));
                offset += 50;
            }
            return list;

            static Vector3 ReadVec(byte[] d, int off)
                => new(BitConverter.ToSingle(d, off + 0),
                       BitConverter.ToSingle(d, off + 4),
                       BitConverter.ToSingle(d, off + 8));
        }

        private static List<(Vector3 A, Vector3 B, Vector3 C)> ParseAscii(byte[] data)
        {
            var list = new List<(Vector3, Vector3, Vector3)>();
            var text = System.Text.Encoding.ASCII.GetString(data);
            var lines = text.Split(new[] { '\r', '\n' }, StringSplitOptions.RemoveEmptyEntries);

            Vector3 v1 = default, v2 = default, v3 = default;
            int vIdx = 0;
            var inv = CultureInfo.InvariantCulture;

            foreach (var raw in lines)
            {
                var line = raw.Trim();
                if (line.StartsWith("vertex ", StringComparison.OrdinalIgnoreCase))
                {
                    var parts = line.Split(' ', StringSplitOptions.RemoveEmptyEntries);
                    if (parts.Length >= 4)
                    {
                        var vx = float.Parse(parts[1], inv);
                        var vy = float.Parse(parts[2], inv);
                        var vz = float.Parse(parts[3], inv);
                        switch (vIdx++)
                        {
                            case 0: v1 = new Vector3(vx, vy, vz); break;
                            case 1: v2 = new Vector3(vx, vy, vz); break;
                            case 2: v3 = new Vector3(vx, vy, vz); break;
                        }
                    }
                }
                else if (line.StartsWith("endfacet", StringComparison.OrdinalIgnoreCase))
                {
                    if (vIdx == 3) list.Add((v1, v2, v3));
                    vIdx = 0;
                }
            }

            if (list.Count == 0)
            {
                var verts = new List<Vector3>();
                foreach (var raw in lines)
                {
                    var line = raw.Trim();
                    if (line.StartsWith("vertex ", StringComparison.OrdinalIgnoreCase))
                    {
                        var parts = line.Split(' ', StringSplitOptions.RemoveEmptyEntries);
                        if (parts.Length >= 4)
                        {
                            verts.Add(new Vector3(
                                float.Parse(parts[1], inv),
                                float.Parse(parts[2], inv),
                                float.Parse(parts[3], inv)));
                        }
                    }
                }
                for (int i = 0; i + 2 < verts.Count; i += 3)
                    list.Add((verts[i], verts[i + 1], verts[i + 2]));
            }

            return list;
        }

        private static List<(Vector3 A, Vector3 B, Vector3 C)> CreateCube(float size)
        {
            float s = size * 0.5f;
            var v = new[]
            {
                new Vector3(-s,-s,-s), new Vector3(s,-s,-s), new Vector3(s,s,-s), new Vector3(-s,s,-s),
                new Vector3(-s,-s, s), new Vector3(s,-s, s), new Vector3(s,s, s), new Vector3(-s,s, s),
            };
            int[] idx = {
                0,1,2, 0,2,3, // back
                4,6,5, 4,7,6, // front
                0,4,5, 0,5,1, // bottom
                3,2,6, 3,6,7, // top
                0,3,7, 0,7,4, // left
                1,5,6, 1,6,2  // right
            };
            var list = new List<(Vector3, Vector3, Vector3)>(12);
            for (int i = 0; i < idx.Length; i += 3) list.Add((v[idx[i]], v[idx[i + 1]], v[idx[i + 2]]));
            return list;
        }

        private static List<(Vector3 A, Vector3 B, Vector3 C)> CreateUvSphere(float r, int seg, int rings)
        {
            var tris = new List<(Vector3, Vector3, Vector3)>();
            for (int y = 0; y < rings; y++)
            {
                float v0 = (float)y / rings;
                float v1 = (float)(y + 1) / rings;
                float phi0 = (v0 * MathF.PI) - MathF.PI / 2f;
                float phi1 = (v1 * MathF.PI) - MathF.PI / 2f;

                for (int x = 0; x < seg; x++)
                {
                    float u0 = (float)x / seg;
                    float u1 = (float)(x + 1) / seg;
                    float th0 = u0 * MathF.Tau;
                    float th1 = u1 * MathF.Tau;

                    Vector3 p00 = Sph(r, th0, phi0);
                    Vector3 p10 = Sph(r, th1, phi0);
                    Vector3 p01 = Sph(r, th0, phi1);
                    Vector3 p11 = Sph(r, th1, phi1);

                    tris.Add((p00, p10, p11));
                    tris.Add((p00, p11, p01));
                }
            }
            return tris;

            static Vector3 Sph(float r, float theta, float phi)
            {
                float cp = MathF.Cos(phi), sp = MathF.Sin(phi);
                float ct = MathF.Cos(theta), st = MathF.Sin(theta);
                return new Vector3(r * cp * ct, r * sp, r * cp * st);
            }
        }

        private static List<(Vector3 A, Vector3 B, Vector3 C)> CreateCylinder(float r, float h, int seg)
        {
            var tris = new List<(Vector3, Vector3, Vector3)>();
            float half = h / 2f;

            Vector3 top = new(0, half, 0), bottom = new(0, -half, 0);

            for (int i = 0; i < seg; i++)
            {
                float t0 = i * MathF.Tau / seg;
                float t1 = (i + 1) * MathF.Tau / seg;
                var v0 = new Vector3(r * MathF.Cos(t0), half, r * MathF.Sin(t0));
                var v1 = new Vector3(r * MathF.Cos(t1), half, r * MathF.Sin(t1));
                var w0 = new Vector3(r * MathF.Cos(t0), -half, r * MathF.Sin(t0));
                var w1 = new Vector3(r * MathF.Cos(t1), -half, r * MathF.Sin(t1));

                // крышка сверху (треугольники к центру)
                tris.Add((top, v0, v1));
                // крышка снизу
                tris.Add((bottom, w1, w0));
                // бок
                tris.Add((v0, w0, w1));
                tris.Add((v0, w1, v1));
            }
            return tris;
        }
        #endregion

        #region Helpers
        private void RecalcSceneBounds()
        {
            _min = new Vector3(float.PositiveInfinity);
            _max = new Vector3(float.NegativeInfinity);
            foreach (var o in SceneObjects.Where(o => o.Visible))
            {
                _min = Vector3.Min(_min, o.Min);
                _max = Vector3.Max(_max, o.Max);
            }
            if (!float.IsFinite(_min.X)) { _min = new Vector3(0); _max = new Vector3(1); }
        }
        #endregion
    }
}
