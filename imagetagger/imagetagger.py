import os
import wx
import math

from wx.lib.pubsub import Publisher

class main_window(wx.Frame):
    def __init__(self, parent, id, title):
        wx.Frame.__init__(self, parent, id, title, size=(1000,600), style=wx.DEFAULT_FRAME_STYLE ^ wx.RESIZE_BORDER)
        
        panel = wx.Panel(self)
              
        sizer1 = wx.BoxSizer(wx.VERTICAL)
        sizer2 = wx.BoxSizer(wx.HORIZONTAL)
        sizer3 = wx.BoxSizer(wx.VERTICAL)
        sizer4 = wx.BoxSizer(wx.HORIZONTAL)
        
        self.btn_loadimage = wx.Button(panel, wx.ID_ANY, "Open an Image")
        self.tbtn_pan = wx.ToggleButton(panel, wx.ID_ANY, "Pan")
        self.tbtn_mark = wx.ToggleButton(panel, wx.ID_ANY, "Mark")
        
        self.Bind(wx.EVT_BUTTON, self.On_btn_loadimage, self.btn_loadimage)
        self.Bind(wx.EVT_TOGGLEBUTTON, self.On_tbtn_pan, self.tbtn_pan)
        self.Bind(wx.EVT_TOGGLEBUTTON, self.On_tbtn_mark, self.tbtn_mark)
        
        sizer4.AddMany([self.btn_loadimage, (7,0), self.tbtn_pan, self.tbtn_mark])
        
        self.t_output = wx.TextCtrl(panel, wx.ID_ANY, "", style=wx.TE_MULTILINE)
        monospacefont = wx.Font(10, wx.TELETYPE, wx.NORMAL, wx.NORMAL, False)
        self.t_output.SetFont(monospacefont)


        Publisher().subscribe(self.append_text, 'append_text')
        sizer3.AddMany([sizer4, (0,10), (self.t_output, 1, wx.EXPAND)])        
        
        self.imagedisplay = ImageWindow(panel)
        #self.wrapper = WrapperWindow(panel)
        sizer2.AddMany([(self.imagedisplay, 1), (20,0), (sizer3, 1, wx.EXPAND)])
        
        
        lbl_descr = wx.StaticText(panel, wx.ID_ANY, "Open an Image, select \"Mark\"-Mode and select slice-points. Right-click to advance to next slice.")
        sizer1.AddMany([lbl_descr, (0,10), (sizer2, 1, wx.EXPAND)])
        sizer0 = wx.BoxSizer(wx.VERTICAL)
        sizer0.Add(sizer1, 1, wx.ALIGN_LEFT|wx.ALL|wx.EXPAND, 5)
        
        #self.SetAutoLayout(True)
        panel.SetSizer(sizer0)
        self.CreateStatusBar()
        #self.Fit()
        Publisher().subscribe(self.change_statusbar, 'change_statusbar')
        self.SetStatusText('Mouse position: 0, 0')
        
        #disable buttons
        self.tbtn_pan.Disable()
        self.tbtn_mark.Disable()
        
        
    def change_statusbar(self, message):
        self.SetStatusText(message.data)
        
    def append_text(self, message):
        self.t_output.AppendText(message.data)
    
    def On_btn_loadimage(self, evt):
        # get current working directory
        dir = os.getcwd()

        # set the initial directory for the demo bitmaps
        #initial_dir = os.path.join(dir, 'bitmaps')

        # open the image browser dialog
        dialog = wx.FileDialog(self, "Open Image", defaultDir=dir, wildcard="All supported types|*.bmp;*.jpg;*.jpeg;*.JPG;*.JPEG;*.png;*.PNG;*.pgm|BMP files (*.bmp)|*.bmp|JPG files (*.jpg)|*.jpg;*.jpeg;*.JPG;*.JPEG|PNG files (*.png)|*.png;*.PNG|PGM files (*.pgm)|*.pgm", style=wx.OPEN)
        #dialog.ChangeFileTypes([('pgm', '*.pgm')])
        dialog.Centre()
        if dialog.ShowModal() == wx.ID_OK:
            # load the selected file
            imagedata = wx.Image(dialog.GetPath(), wx.BITMAP_TYPE_ANY)
            #start with pan-mode
            self.tbtn_pan.SetValue(True)
            self.On_tbtn_pan(evt)
            #enable buttons
            self.tbtn_pan.Enable()
            self.tbtn_mark.Enable()
            self.imagedisplay.LoadImage(imagedata)
        dialog.Destroy()
                
            
    def On_tbtn_pan(self, evt):
        cursor_to_use = wx.StockCursor(wx.CURSOR_ARROW)
        self.imagedisplay.SetCursor(cursor_to_use)
        self.imagedisplay.Markmode = False
        self.untoggle_buttons(self.tbtn_pan)
        
    def On_tbtn_mark(self, evt):
        cursor_to_use = wx.StockCursor(wx.CURSOR_BULLSEYE)
        self.imagedisplay.SetCursor(cursor_to_use)
        self.imagedisplay.Markmode = True
        self.untoggle_buttons(self.tbtn_mark)

    def untoggle_buttons(self, parent=None):        
        self.tbtn_pan.SetValue(False)
        self.tbtn_mark.SetValue(False)
        if parent is not None:
            parent.SetValue(True)
            
class WrapperWindow(wx.Window):
    #not used
    #this is a window which gets expaned and propagates its size. nothing more
    def __init__(self, parent):
        wx.Window.__init__(self, parent, wx.ID_ANY, style=wx.NO_BORDER)
        panel = wx.Panel(self)
        sizer = wx.BoxSizer(wx.VERTICAL)
        imagedisplay = ImageWindow(panel)
        sizer.Add(imagedisplay)
        panel.SetSizer(sizer)
                
class ImageWindow(wx.ScrolledWindow):
    def __init__(self, parent):
        wx.ScrolledWindow.__init__(self, parent, wx.ID_ANY, style=wx.FULL_REPAINT_ON_RESIZE|wx.NO_BORDER)
        self.SetVirtualSize(self.GetClientSize())        
        
        self.Image = wx.EmptyImage(0, 0)
        self.OutputBuffer = wx.EmptyImage(0, 0)
        self.Imagewidth = 1
        self.Imageheight = 1
        self.Zoom = 32.0
        client_width = 480
        client_height = 512
        client_width = client_width - int(client_width % self.Zoom)
        client_height = client_height - int(client_height % self.Zoom)
        self.SetClientSize((client_width,client_height)) #dirty hack -- has to correspond to Zoom
        self.Width = client_width
        self.Height = client_height
        self.ZoomAbsolute = 1.0
        self.Ppu = 32 #pixels per scrollunit
        self.Color = "red"
        self.Markmode = False
        self.PointRect = None
        #Binders
        self.Bind(wx.EVT_PAINT, self.OnPaint)
        self.Bind(wx.EVT_SIZE, self.OnSize) #after startup this is executed before EVT_PAINT
        self.Bind(wx.EVT_MOTION, self.OnMouseMove)
        self.Bind(wx.EVT_LEFT_UP, self.OnLeftMouseClick)
        self.Bind(wx.EVT_RIGHT_UP, self.OnRightMouseClick)
        
        
        #setup output window
        Publisher().sendMessage(('append_text'), "    (")
        self.Outputcount = 0
        self.FirstSlice = True
        self.writeNewSlicetoOutput()
        
    ### CORE FUNCTIONS ###
    
    def redraw(self):
        #the goal is to draw an image at self.Zoom-times its original size   
        top_x, top_y = self.GetViewStartAbsolute()
        top_x = top_x / self.Zoom
        top_y = top_y / self.Zoom
        width = self.Width / self.Zoom
        height = self.Height / self.Zoom
        rect = (int(top_x), int(top_y), int(width), int(height))
        roi = self.OutputBuffer.GetSubImage(rect)
        width_before = roi.GetWidth()
        height_before = roi.GetHeight()
        roi.Rescale(self.Width, self.Height)
        width_after = roi.GetWidth()
        height_after = roi.GetHeight()
        roi = self.DrawRect(roi)
        return roi
    
    def FitClient(self):
        #make the image fit the client-size with its smaller edge
        if self.Imagewidth < self.Imageheight:
            zoom = self.Imagewidth / float(self.Width)
        else:
            zoom = self.Imageheight / float(self.Height)
        zoom = 1.0
        self.ZoomAbsolute = self.Zoom / zoom
        width = int(math.floor(self.Imagewidth / float(zoom)))
        height = int(math.floor(self.Imageheight / float(zoom)))
        width_before = self.Image.GetWidth()
        self.OutputBuffer = self.Image.Scale(width, height)
        width_after = self.OutputBuffer.GetWidth()
        vwidth = width * self.Zoom
        vheight = height * self.Zoom
        self.SetVirtualSize((vwidth, vheight))
    
    ### HELPER FUNCTIONS ###
    
        
    def DrawPoint(self, point, color):
        #takes self.Image, draws a point onto it
        bitmap = wx.BitmapFromImage(self.Image)        
        memDC = wx.MemoryDC()
        memDC.SelectObject(bitmap)
        pen = wx.Pen(color, 1)
        memDC.SetPen(pen)
        memDC.DrawPoint(point[0], point[1])
        memDC.SelectObject(wx.NullBitmap) #free the memDC
        self.Image = wx.ImageFromBitmap(bitmap)
        self.FitClient()
        self.Refresh()
        
    def DrawRect(self, image):
        if self.PointRect is None:
            return image
        bitmap = wx.BitmapFromImage(image)
        memDC = wx.MemoryDC()
        memDC.SelectObject(bitmap)
        pen = wx.Pen("blue", 1, style=wx.DOT)
        memDC.SetPen(pen)
        memDC.SetBrush(wx.TRANSPARENT_BRUSH)
        memDC.DrawRectangle(self.PointRect[0] * self.ZoomAbsolute, self.PointRect[1] * self.ZoomAbsolute, 1 * self.ZoomAbsolute, 1 * self.ZoomAbsolute)
        memDC.SelectObject(wx.NullBitmap)
        image = wx.ImageFromBitmap(bitmap) 
        return image
        
    def GetViewStartAbsolute(self):
        x,y = self.GetViewStart()
        x = x * self.Ppu
        y = y * self.Ppu
        return (x,y)
        
    def GetMousePosAbsolute(self, event):
        top_x, top_y = self.GetViewStartAbsolute()
        pos_x = int(math.floor((top_x + event.GetX()) / self.ZoomAbsolute))
        pos_y = int(math.floor((top_y + event.GetY()) / self.ZoomAbsolute))
        if pos_x <= self.Imagewidth and pos_y <= self.Imageheight:
            return (pos_x, pos_y)
        else:
            return None
            
    def GetMousePosRelative(self, event):
        pos_x = int(event.GetX() / self.ZoomAbsolute)
        pos_y = int(event.GetY() / self.ZoomAbsolute)
        if pos_x <= self.Imagewidth-1 and pos_y <= self.Imageheight-1:
            return (pos_x, pos_y)
        else:
            return None
            
    def AdjustAspectRatio(self):
        return #this does not work yet
        #adjust aspect ratio
        ImageAspectRatio = self.Imagewidth / float(self.Imageheight)
        print "Ratio: " + str(ImageAspectRatio)
        self.Height = self.Width / ImageAspectRatio
                
    def writePointtoOutput(self, pos):
        line1 = ""
        if self.FirstPoint == False:
            line1 = ",\n"
        else:
            line1 = "\n"
        x = "(" + str(pos[0])
        y = str(pos[1])
        formattedx = "%38s" % x
        formattedy = str(y) + ")"
        line1 = line1 + formattedx + "," + formattedy
        Publisher().sendMessage(('append_text'), line1)
        self.FirstPoint = False
        
    def writeNewSlicetoOutput(self):
        self.FirstPoint = True
        
        line1 = ""
        if self.FirstSlice == False:
            line1 = "\n				  )),\n" + "	    Threshold_Lower => -1,\n" + "	    Threshold_Upper => -1,\n" + "	    Link_Slice_Other_Rail => -1),\n"
        self.Outputcount = self.Outputcount +1
        incrementer = "%6s" % self.Outputcount
        line1 = line1 + "\n" + incrementer + " => ( Point => ( new Points'("
        Publisher().sendMessage(('append_text'), line1)
        self.FirstSlice = False
        
    ### EVENT STUFF ###
    ## externally triggered events ###

    def LoadImage(self, image):
        self.Image = image
        self.Imagewidth = self.Image.GetWidth()
        self.Imageheight = self.Image.GetHeight()
        self.AdjustAspectRatio()
        self.FitClient()
        self.Scroll(0,0)
        self.SetScrollRate(self.Ppu, self.Ppu)
        self.Refresh()

    ### internally-triggered events ###
    
    def OnPaint(self, event):
        imagetodraw = self.redraw()
        dc = wx.PaintDC(self)
        dc.DrawBitmap(wx.BitmapFromImage(imagetodraw), 0, 0)
        
    def OnSize(self, event):
        self.Width, self.Height = self.GetClientSizeTuple()
        self.Width = 480
        self.Height = 512
        self.AdjustAspectRatio()
        self.FitClient()
        self.Refresh()
        #self.initscale()
        
    def OnMouseMove(self, event):
        pos = self.GetMousePosAbsolute(event)
        if pos is not None:
            value = "Mouse position: " + str(pos[0]) +  ", " + str(pos[1])
            Publisher().sendMessage(('change_statusbar'), value)
            if self.Markmode == True:
                self.PointRect = self.GetMousePosRelative(event)
                self.Refresh()
        else:
            self.PointRect = None
        
    def OnLeftMouseClick(self, event):
        if self.Markmode == False:
            return
        pos = self.GetMousePosAbsolute(event)
        if pos is not None:
            self.writePointtoOutput(pos)
            self.DrawPoint(pos, self.Color)
            
    def OnRightMouseClick(self, event):
        if self.Markmode == False:
            return
        self.writeNewSlicetoOutput()
        if self.Color == "red":
            self.Color = "yellow"
        else:
            self.Color = "red"
        
class ImageTagger(wx.App):

    # wxWindows calls this method to initialize the application
    def OnInit(self):

        # Create an instance of our customized Frame class
        frame = main_window(None, wx.ID_ANY, "ImageTagger")
        frame.Show(True)

        # Tell wxWindows that this is our main window
        self.SetTopWindow(frame)

        # Return a success flag
        return True
        
        
if __name__=='__main__':    
    app = ImageTagger(0)     # Create an instance of the application class
    app.MainLoop()     # Tell it to start processing events
