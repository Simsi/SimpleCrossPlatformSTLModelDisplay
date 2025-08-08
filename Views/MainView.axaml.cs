using Avalonia;
using Avalonia.Controls;
using Avalonia.Input;
using Avalonia.Interactivity;
using Avalonia.Platform.Storage;
using System.Threading.Tasks;
using TestGPT5.Classes;


namespace TestGPT5.Views
{
    public partial class MainView : UserControl
    {
        public MainView()
        {
            InitializeComponent();

            // подписываем события вместо PointerPressed="..." в XAML
            BtnCube.PointerPressed += Primitive_PointerPressed;
            BtnSphere.PointerPressed += Primitive_PointerPressed;
            BtnCylinder.PointerPressed += Primitive_PointerPressed;
        }

        private async void OpenStl_Click(object? sender, RoutedEventArgs e)
        {
            var top = TopLevel.GetTopLevel(this);
            if (top is null) return;

            var files = await top.StorageProvider.OpenFilePickerAsync(new FilePickerOpenOptions
            {
                AllowMultiple = false,
                Title = "Выберите STL файл",
                FileTypeFilter = new[] { new FilePickerFileType("STL files") { Patterns = new[] { "*.stl" } } }
            });

            if (files.Count == 1)
            {
                await using var s = await files[0].OpenReadAsync();
                var viewer = this.FindControl<StlView3D>("Viewer");
                if (viewer is null) return;
                await viewer.LoadFromStreamAsync(s);
                viewer.Focus();
            }
        }


    private async void Primitive_PointerPressed(object? sender, PointerPressedEventArgs e)
    {
        if (sender is Button btn && btn.Tag is string tag)
        {
            var data = new DataObject();
            data.Set(DataFormats.Text, tag); // <-- вместо кастомного ключа
            await DragDrop.DoDragDrop(e, data, DragDropEffects.Copy);
        }
    }
}
}
