package com.verr1.controlcraft.content.gui.screens;

import com.mojang.blaze3d.platform.InputConstants;
import com.simibubi.create.foundation.gui.AbstractSimiScreen;
import com.verr1.controlcraft.content.gui.widgets.FormattedLabel;
import com.verr1.controlcraft.foundation.input.ControlChairAxis;
import com.verr1.controlcraft.foundation.input.ControlChairBindingsProfile;
import com.verr1.controlcraft.foundation.input.ControlChairBindingsProfile.AnalogBinding;
import com.verr1.controlcraft.foundation.input.ControlChairBindingsProfile.AxisDirection;
import com.verr1.controlcraft.foundation.input.ControlChairBindingsProfile.DigitalBinding;
import com.verr1.controlcraft.foundation.input.ControlChairBindingsProfile.DirectionBinding;
import com.verr1.controlcraft.foundation.input.ControlChairBindingsStore;
import com.verr1.controlcraft.foundation.input.ControlChairButton;
import com.verr1.controlcraft.foundation.input.ControlChairGamepadManager;
import com.verr1.controlcraft.foundation.managers.ClientControlChairManager;
import net.minecraft.Util;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.AbstractSliderButton;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.Tooltip;
import net.minecraft.client.gui.layouts.GridLayout;
import net.minecraft.network.chat.CommonComponents;
import net.minecraft.network.chat.Component;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import org.jetbrains.annotations.Nullable;
import org.lwjgl.glfw.GLFW;

import java.util.Locale;

@OnlyIn(Dist.CLIENT)
public class ControlChairBindingsScreen extends AbstractSimiScreen {
    private static final int WINDOW_WIDTH = 464;
    private static final int WINDOW_HEIGHT = 268;
    private static final int ROWS_PER_PAGE = 8;
    private static final int TITLE_COLOR = 0xF1F4FB;
    private static final int LABEL_COLOR = 0xD7DEEB;
    private static final int HEADER_COLOR = 0xAAB4C7;
    private static final int FOOTER_COLOR = 0xBDC6D8;
    private static final int BORDER_COLOR = 0xFF101216;
    private static final int PANEL_TOP_COLOR = 0xFF383C44;
    private static final int PANEL_BOTTOM_COLOR = 0xFF23262C;
    private static final int TITLE_BAR_TOP = 0xFF4D5360;
    private static final int TITLE_BAR_BOTTOM = 0xFF3A3E47;
    private static final int SECTION_COLOR = 0x66111217;
    private static final int DIVIDER_COLOR = 0xFF676E7D;
    private static final int ACCENT_COLOR = 0xFF6F7752;
    private static final int SIDE_PADDING = 12;
    private static final int TOP_CONTROLS_Y = 24;
    private static final int HEADER_Y = 50;
    private static final int ROWS_Y = 62;
    private static final int FOOTER_Y = 236;
    private static final int BUTTON_HEIGHT = 15;
    private static final int ROW_SPACING = 4;
    private static final int COLUMN_SPACING = 6;
    private static final int LABEL_WIDTH = 42;
    private static final int SLOT_BUTTON_WIDTH = 102;
    private static final int CLEAR_BUTTON_WIDTH = 18;
    private static final int SENSITIVITY_WIDTH = BUTTON_HEIGHT * 3;
    private static final int TAB_AXES_WIDTH = 62;
    private static final int TAB_BUTTONS_WIDTH = 78;
    private static final int PAGE_ARROW_WIDTH = 18;
    private static final int PAGE_LABEL_WIDTH = 58;
    private static final int RESET_BUTTON_WIDTH = 74;
    private static final int DONE_BUTTON_WIDTH = 60;
    private static final int BINDING_TEXT_PADDING = 6;
    private static final int SLIDER_TRACK_HEIGHT = 4;
    private static final int SLIDER_HANDLE_WIDTH = 6;
    private static final int SLIDER_HANDLE_HEIGHT = 9;
    private static final int SLIDER_TEXT_COLOR = 0xF1F4FB;
    private static final int SLIDER_TEXT_DISABLED_COLOR = 0x7E879A;
    private static final int SLIDER_TRACK_COLOR = 0xFF17191F;
    private static final int SLIDER_TRACK_FILL_COLOR = 0xFF6F7752;
    private static final int SLIDER_TRACK_FILL_HOVER_COLOR = 0xFF899364;
    private static final int SLIDER_HANDLE_COLOR = 0xFFE7EDF7;
    private static final int SLIDER_HANDLE_DISABLED_COLOR = 0xFF8E97A8;
    private static final int MARQUEE_HOLD_MS = 420;
    private static final int MARQUEE_PIXELS_PER_SECOND = 42;
    private static final double MOUSE_CAPTURE_THRESHOLD = 8.0;
    private static final double GAMEPAD_AXIS_CAPTURE_THRESHOLD = 0.45;
    private final ControlChairBindingsProfile workingCopy = ControlChairBindingsStore.current().copy();
    private final RowContext[] rows = new RowContext[ROWS_PER_PAGE];

    private GridLayout tabsLayout;
    private GridLayout pageLayout;
    private GridLayout headerLayout;
    private GridLayout footerLayout;

    private Button axesTabButton;
    private Button buttonsTabButton;
    private Button previousPageButton;
    private Button nextPageButton;
    private Button resetButton;
    private Button doneButton;

    private FormattedLabel pageIndicatorLabel;
    private FormattedLabel slotHeaderLabel;
    private FormattedLabel primaryHeaderLabel;
    private FormattedLabel secondaryHeaderLabel;
    private FormattedLabel sensitivityHeaderLabel;

    private Category category = Category.AXES;
    private int page = 0;
    private @Nullable CaptureTarget captureTarget;
    private ControlChairGamepadManager.Snapshot captureBaseline = ControlChairGamepadManager.Snapshot.disconnected();
    private boolean ignoreNextMouseClick = false;
    private boolean ignoreNextMouseMove = false;
    private double lastMouseX = 0.0;
    private double lastMouseY = 0.0;

    public ControlChairBindingsScreen() {
        super(Component.empty());
    }

    @Override
    protected void init() {
        setWindowSize(WINDOW_WIDTH, WINDOW_HEIGHT);
        super.init();

        createControls();
        layoutControls();
        refreshRows();
    }

    @Override
    public void tick() {
        super.tick();

        if (!ClientControlChairManager.hasActiveChair()) {
            onClose();
            return;
        }

        if (captureTarget != null) {
            pollGamepadCapture();
        }
    }

    @Override
    public boolean keyPressed(int keyCode, int scanCode, int modifiers) {
        if (captureTarget != null) {
            if (keyCode == GLFW.GLFW_KEY_ESCAPE) {
                finishCapture();
                return true;
            }
            if (keyCode == GLFW.GLFW_KEY_DELETE || keyCode == GLFW.GLFW_KEY_BACKSPACE) {
                clearCaptureTargetBinding();
                return true;
            }
            if (captureTarget.acceptsDigital()) {
                applyCapturedDigital(DigitalBinding.key(keyCode));
                return true;
            }
            return true;
        }

        if (keyCode == GLFW.GLFW_KEY_ESCAPE) {
            onClose();
            return true;
        }

        return super.keyPressed(keyCode, scanCode, modifiers);
    }

    @Override
    public boolean mouseClicked(double mouseX, double mouseY, int button) {
        boolean handled = super.mouseClicked(mouseX, mouseY, button);
        if (ignoreNextMouseClick) {
            ignoreNextMouseClick = false;
            return true;
        }
        if (captureTarget != null && captureTarget.acceptsDigital()) {
            applyCapturedDigital(DigitalBinding.mouse(button));
            return true;
        }
        return handled;
    }

    @Override
    public void mouseMoved(double mouseX, double mouseY) {
        super.mouseMoved(mouseX, mouseY);
        double dx = mouseX - lastMouseX;
        double dy = mouseY - lastMouseY;
        lastMouseX = mouseX;
        lastMouseY = mouseY;

        if (captureTarget != null && captureTarget.acceptsAnalog()) {
            if (ignoreNextMouseMove) {
                ignoreNextMouseMove = false;
                return;
            }

            if (Math.abs(dx) >= Math.abs(dy) && Math.abs(dx) >= MOUSE_CAPTURE_THRESHOLD) {
                applyCapturedAnalog(dx > 0.0
                        ? AnalogBinding.mouseYawPositive()
                        : AnalogBinding.mouseYawNegative());
                return;
            }

            if (Math.abs(dy) >= MOUSE_CAPTURE_THRESHOLD) {
                applyCapturedAnalog(dy > 0.0
                        ? AnalogBinding.mousePitchPositive()
                        : AnalogBinding.mousePitchNegative());
            }
        }
    }

    @Override
    public void onClose() {
        ControlChairBindingsStore.replace(workingCopy);
        super.onClose();
    }

    @Override
    protected void renderWindow(GuiGraphics graphics, int mouseX, int mouseY, float partialTicks) {
        renderPanelBackground(graphics);
        graphics.drawCenteredString(font, Component.literal("Control Chair Bindings"), guiLeft + windowWidth / 2, guiTop + 10, TITLE_COLOR);

        int contentLeft = contentLeft();
        int footerButtonsX = contentLeft + contentWidth() - layoutWidth(footerLayout);
        int footerTextWidth = Math.max(0, footerButtonsX - contentLeft - 6);
        graphics.drawString(font, trimText(footerText(), footerTextWidth), contentLeft, guiTop + FOOTER_Y + 6, FOOTER_COLOR, false);
    }

    @Override
    protected void renderWindowForeground(GuiGraphics graphics, int mouseX, int mouseY, float partialTicks) {
        renderBindingButtonText(graphics);
        super.renderWindowForeground(graphics, mouseX, mouseY, partialTicks);
    }

    private void createControls() {
        tabsLayout = new GridLayout();
        pageLayout = new GridLayout();
        headerLayout = new GridLayout();
        footerLayout = new GridLayout();

        axesTabButton = button(Component.literal("Axes"), button -> switchCategory(Category.AXES), TAB_AXES_WIDTH);
        buttonsTabButton = button(Component.literal("Buttons"), button -> switchCategory(Category.BUTTONS), TAB_BUTTONS_WIDTH);
        previousPageButton = button(Component.literal("<"), button -> changePage(-1), PAGE_ARROW_WIDTH);
        nextPageButton = button(Component.literal(">"), button -> changePage(1), PAGE_ARROW_WIDTH);
        resetButton = button(Component.literal("Defaults"), button -> resetBindings(), RESET_BUTTON_WIDTH);
        doneButton = button(CommonComponents.GUI_DONE, button -> onClose(), DONE_BUTTON_WIDTH);

        pageIndicatorLabel = headerLabel(Component.literal(pageLabel()), PAGE_LABEL_WIDTH);
        slotHeaderLabel = headerLabel(Component.literal("Slot"), LABEL_WIDTH);
        primaryHeaderLabel = headerLabel(primaryHeader(), SLOT_BUTTON_WIDTH);
        secondaryHeaderLabel = headerLabel(secondaryHeader(), SLOT_BUTTON_WIDTH);
        sensitivityHeaderLabel = headerLabel(sensitivityHeader(), SENSITIVITY_WIDTH);

        tabsLayout.addChild(axesTabButton, 0, 0);
        tabsLayout.addChild(buttonsTabButton, 0, 1);
        tabsLayout.columnSpacing(COLUMN_SPACING);

        pageLayout.addChild(previousPageButton, 0, 0);
        pageLayout.addChild(pageIndicatorLabel, 0, 1);
        pageLayout.addChild(nextPageButton, 0, 2);
        pageLayout.columnSpacing(COLUMN_SPACING);

        headerLayout.addChild(slotHeaderLabel, 0, 0);
        headerLayout.addChild(primaryHeaderLabel, 0, 1);
        headerLayout.addChild(spacerLabel(CLEAR_BUTTON_WIDTH), 0, 2);
        headerLayout.addChild(secondaryHeaderLabel, 0, 3);
        headerLayout.addChild(spacerLabel(CLEAR_BUTTON_WIDTH), 0, 4);
        headerLayout.addChild(sensitivityHeaderLabel, 0, 5);
        headerLayout.columnSpacing(COLUMN_SPACING);

        footerLayout.addChild(resetButton, 0, 0);
        footerLayout.addChild(doneButton, 0, 1);
        footerLayout.columnSpacing(COLUMN_SPACING);

        addLayoutWidgets(tabsLayout);
        addLayoutWidgets(pageLayout);
        addLayoutWidgets(headerLayout);
        addLayoutWidgets(footerLayout);

        for (int row = 0; row < ROWS_PER_PAGE; row++) {
            rows[row] = createRow(row);
            addLayoutWidgets(rows[row].layout);
        }
    }

    private void layoutControls() {
        int contentLeft = contentLeft();
        int contentWidth = contentWidth();

        positionLayout(tabsLayout, contentLeft, guiTop + TOP_CONTROLS_Y);
        positionLayout(pageLayout, contentLeft + contentWidth - layoutWidth(pageLayout), guiTop + TOP_CONTROLS_Y);
        positionLayout(headerLayout, contentLeft, guiTop + HEADER_Y);
        positionLayout(footerLayout, contentLeft + contentWidth - layoutWidth(footerLayout), guiTop + FOOTER_Y);

        for (int row = 0; row < ROWS_PER_PAGE; row++) {
            int y = guiTop + ROWS_Y + row * (BUTTON_HEIGHT + ROW_SPACING);
            positionLayout(rows[row].layout, contentLeft, y);
        }
    }

    private RowContext createRow(int rowIndex) {
        RowContext context = new RowContext();
        context.labelWidget = rowLabel();
        context.primaryButton = button(Component.empty(), button -> runRowAction(rowIndex, Slot.PRIMARY), SLOT_BUTTON_WIDTH);
        context.clearPrimaryButton = button(Component.literal("X"), button -> runRowAction(rowIndex, Slot.CLEAR_PRIMARY), CLEAR_BUTTON_WIDTH);
        context.secondaryButton = button(Component.empty(), button -> runRowAction(rowIndex, Slot.SECONDARY), SLOT_BUTTON_WIDTH);
        context.clearSecondaryButton = button(Component.literal("X"), button -> runRowAction(rowIndex, Slot.CLEAR_SECONDARY), CLEAR_BUTTON_WIDTH);
        context.sensitivitySlider = new SensitivitySlider();

        context.layout = new GridLayout();
        context.layout.addChild(context.labelWidget, 0, 0);
        context.layout.addChild(context.primaryButton, 0, 1);
        context.layout.addChild(context.clearPrimaryButton, 0, 2);
        context.layout.addChild(context.secondaryButton, 0, 3);
        context.layout.addChild(context.clearSecondaryButton, 0, 4);
        context.layout.addChild(context.sensitivitySlider, 0, 5);
        context.layout.columnSpacing(COLUMN_SPACING);
        return context;
    }

    private Button button(Component message, Button.OnPress onPress, int width) {
        return Button.builder(message, onPress)
                .bounds(0, 0, width, BUTTON_HEIGHT)
                .build();
    }

    private FormattedLabel rowLabel() {
        return new FormattedLabel(0, 0, Component.empty())
                .withTextStyle(style -> style.withColor(LABEL_COLOR))
                .withWidth(LABEL_WIDTH);
    }

    private FormattedLabel headerLabel(Component text, int width) {
        return new FormattedLabel(0, 0, text)
                .withTextStyle(style -> style.withColor(HEADER_COLOR))
                .withWidth(width);
    }

    private FormattedLabel spacerLabel(int width) {
        return new FormattedLabel(0, 0, Component.empty()).withWidth(width);
    }

    private void addLayoutWidgets(GridLayout layout) {
        layout.visitWidgets(this::addRenderableWidget);
    }

    private void positionLayout(GridLayout layout, int x, int y) {
        layout.arrangeElements();
        layout.setX(x);
        layout.setY(y);
        layout.arrangeElements();
    }

    private int layoutWidth(GridLayout layout) {
        layout.arrangeElements();
        return layout.getWidth();
    }

    private int contentWidth() {
        return layoutWidth(headerLayout);
    }

    private int contentLeft() {
        return guiLeft + Math.max(SIDE_PADDING, (windowWidth - contentWidth()) / 2);
    }

    private void switchCategory(Category nextCategory) {
        if (category == nextCategory) {
            return;
        }
        category = nextCategory;
        page = 0;
        finishCapture();
        refreshRows();
    }

    private void changePage(int delta) {
        int nextPage = Math.max(0, Math.min(page + delta, totalPages() - 1));
        if (nextPage == page) {
            return;
        }
        page = nextPage;
        finishCapture();
        refreshRows();
    }

    private void resetBindings() {
        ControlChairBindingsProfile defaults = ControlChairBindingsProfile.defaults();
        for (AxisDirection direction : AxisDirection.VALUES) {
            workingCopy.setDirectionAnalogBinding(direction, defaults.directionBinding(direction).analogBinding());
            workingCopy.setDirectionButtonBinding(direction, defaults.directionBinding(direction).buttonBinding());
        }
        for (ControlChairAxis axis : ControlChairAxis.VALUES) {
            workingCopy.setAxisSensitivity(axis, defaults.axisSensitivity(axis));
        }
        for (ControlChairButton button : ControlChairButton.VALUES) {
            workingCopy.setButtonBindings(button, defaults.buttonBindings(button));
        }
        finishCapture();
        refreshRows();
    }

    private void runRowAction(int rowIndex, Slot slot) {
        RowContext context = rows[rowIndex];
        if (!context.visible) {
            return;
        }

        switch (slot) {
            case PRIMARY -> context.primaryAction.run();
            case CLEAR_PRIMARY -> context.clearPrimaryAction.run();
            case SECONDARY -> context.secondaryAction.run();
            case CLEAR_SECONDARY -> context.clearSecondaryAction.run();
        }
    }

    private void refreshRows() {
        axesTabButton.active = category != Category.AXES;
        buttonsTabButton.active = category != Category.BUTTONS;
        previousPageButton.active = page > 0;
        nextPageButton.active = page + 1 < totalPages();

        pageIndicatorLabel.setTextOnly(styled(pageLabel(), HEADER_COLOR));
        primaryHeaderLabel.setTextOnly(styled(primaryHeader(), HEADER_COLOR));
        secondaryHeaderLabel.setTextOnly(styled(secondaryHeader(), HEADER_COLOR));
        sensitivityHeaderLabel.setTextOnly(styled(sensitivityHeader(), HEADER_COLOR));

        for (int row = 0; row < ROWS_PER_PAGE; row++) {
            RowContext context = rows[row];
            int index = page * ROWS_PER_PAGE + row;

            if (index >= entryCount()) {
                context.clear();
                applyRowContext(context);
                continue;
            }

            if (category == Category.AXES) {
                configureAxisRow(context, AxisDirection.VALUES[index]);
            } else {
                configureButtonRow(context, ControlChairButton.VALUES[index]);
            }

            applyRowContext(context);
        }
    }

    private void configureAxisRow(RowContext context, AxisDirection direction) {
        DirectionBinding binding = workingCopy.directionBinding(direction);
        context.visible = true;
        context.label = styled(axisRowLabel(direction), LABEL_COLOR);
        context.primaryLabel = analogBindingLabel(binding.analogBinding());
        context.secondaryLabel = digitalBindingLabel(binding.buttonBinding());
        context.primaryAction = () -> beginCapture(CaptureTarget.axisAnalog(direction));
        context.clearPrimaryAction = () -> {
            workingCopy.setDirectionAnalogBinding(direction, AnalogBinding.unbound());
            finishCapture();
            refreshRows();
        };
        context.secondaryAction = () -> beginCapture(CaptureTarget.axisDigital(direction));
        context.clearSecondaryAction = () -> {
            workingCopy.setDirectionButtonBinding(direction, DigitalBinding.unbound());
            finishCapture();
            refreshRows();
        };
        context.showSensitivity = direction.positive();
        context.sensitivityAxis = direction.axis();
        context.sensitivity = workingCopy.axisSensitivity(direction.axis());
    }

    private void configureButtonRow(RowContext context, ControlChairButton button) {
        var bindings = workingCopy.buttonBindings(button);
        context.visible = true;
        context.label = styled(buttonRowLabel(button), LABEL_COLOR);
        context.primaryLabel = digitalBindingLabel(bindings.primary());
        context.secondaryLabel = digitalBindingLabel(bindings.secondary());
        context.primaryAction = () -> beginCapture(CaptureTarget.buttonPrimary(button));
        context.clearPrimaryAction = () -> {
            workingCopy.setButtonPrimaryBinding(button, DigitalBinding.unbound());
            finishCapture();
            refreshRows();
        };
        context.secondaryAction = () -> beginCapture(CaptureTarget.buttonSecondary(button));
        context.clearSecondaryAction = () -> {
            workingCopy.setButtonSecondaryBinding(button, DigitalBinding.unbound());
            finishCapture();
            refreshRows();
        };
        context.showSensitivity = false;
        context.sensitivityAxis = null;
        context.sensitivity = ControlChairBindingsProfile.DEFAULT_SENSITIVITY;
    }

    private void applyRowContext(RowContext context) {
        context.labelWidget.visible = context.visible;
        context.primaryButton.visible = context.visible;
        context.clearPrimaryButton.visible = context.visible;
        context.secondaryButton.visible = context.visible;
        context.clearSecondaryButton.visible = context.visible;
        context.sensitivitySlider.visible = context.visible && context.showSensitivity;

        context.primaryButton.active = context.visible;
        context.clearPrimaryButton.active = context.visible;
        context.secondaryButton.active = context.visible;
        context.clearSecondaryButton.active = context.visible;
        context.sensitivitySlider.active = context.visible && context.showSensitivity;

        context.labelWidget.setTextOnly(context.label);
        context.primaryButton.setMessage(CommonComponents.EMPTY);
        context.primaryButton.setTooltip(context.visible ? Tooltip.create(context.primaryLabel) : null);
        context.secondaryButton.setMessage(CommonComponents.EMPTY);
        context.secondaryButton.setTooltip(context.visible ? Tooltip.create(context.secondaryLabel) : null);

        if (context.showSensitivity && context.sensitivityAxis != null) {
            context.sensitivitySlider.bind(context.sensitivityAxis, context.sensitivity);
        } else {
            context.sensitivitySlider.clearBinding();
        }
    }

    private void beginCapture(CaptureTarget nextTarget) {
        captureTarget = nextTarget;
        captureBaseline = ControlChairGamepadManager.snapshot().copy();
        ignoreNextMouseClick = true;
        ignoreNextMouseMove = true;
    }

    private void finishCapture() {
        captureTarget = null;
        ignoreNextMouseClick = false;
        ignoreNextMouseMove = false;
    }

    private void clearCaptureTargetBinding() {
        if (captureTarget == null) {
            return;
        }

        switch (captureTarget.slotType) {
            case AXIS_ANALOG -> workingCopy.setDirectionAnalogBinding(captureTarget.axisDirection, AnalogBinding.unbound());
            case AXIS_DIGITAL -> workingCopy.setDirectionButtonBinding(captureTarget.axisDirection, DigitalBinding.unbound());
            case BUTTON_PRIMARY -> workingCopy.setButtonPrimaryBinding(captureTarget.button, DigitalBinding.unbound());
            case BUTTON_SECONDARY -> workingCopy.setButtonSecondaryBinding(captureTarget.button, DigitalBinding.unbound());
        }

        finishCapture();
        refreshRows();
    }

    private void applyCapturedDigital(DigitalBinding binding) {
        if (captureTarget == null) {
            return;
        }

        switch (captureTarget.slotType) {
            case AXIS_DIGITAL -> workingCopy.setDirectionButtonBinding(captureTarget.axisDirection, binding);
            case BUTTON_PRIMARY -> workingCopy.setButtonPrimaryBinding(captureTarget.button, binding);
            case BUTTON_SECONDARY -> workingCopy.setButtonSecondaryBinding(captureTarget.button, binding);
            case AXIS_ANALOG -> {
                return;
            }
        }

        finishCapture();
        refreshRows();
    }

    private void applyCapturedAnalog(AnalogBinding binding) {
        if (captureTarget == null || !captureTarget.acceptsAnalog()) {
            return;
        }

        workingCopy.setDirectionAnalogBinding(captureTarget.axisDirection, binding);
        finishCapture();
        refreshRows();
    }

    private void pollGamepadCapture() {
        if (captureTarget == null) {
            return;
        }

        ControlChairGamepadManager.Snapshot snapshot = ControlChairGamepadManager.snapshot();
        if (!snapshot.connected()) {
            return;
        }

        if (captureTarget.acceptsDigital()) {
            for (int button = 0; button < snapshot.buttons().length; button++) {
                if (snapshot.buttonDown(button) && !captureBaseline.buttonDown(button)) {
                    applyCapturedDigital(DigitalBinding.gamepad(button));
                    return;
                }
            }
            return;
        }

        AnalogBinding gamepadAxisBinding = detectGamepadAxisBinding(snapshot, captureBaseline);
        if (gamepadAxisBinding != null) {
            applyCapturedAnalog(gamepadAxisBinding);
        }
    }

    private @Nullable AnalogBinding detectGamepadAxisBinding(
            ControlChairGamepadManager.Snapshot snapshot,
            ControlChairGamepadManager.Snapshot baseline
    ) {
        double bestDelta = GAMEPAD_AXIS_CAPTURE_THRESHOLD;
        AnalogBinding bestBinding = null;

        for (int axis = 0; axis < snapshot.axes().length; axis++) {
            double current = normalizedAxisValue(snapshot, axis);
            double previous = normalizedAxisValue(baseline, axis);
            double delta = current - previous;
            double magnitude = Math.abs(delta);

            if (magnitude < bestDelta) {
                continue;
            }

            if (delta >= 0.0 || isTriggerAxis(snapshot, axis)) {
                bestBinding = AnalogBinding.gamepadAxisPositive(axis);
                bestDelta = magnitude;
            } else {
                bestBinding = AnalogBinding.gamepadAxisNegative(axis);
                bestDelta = magnitude;
            }
        }

        return bestBinding;
    }

    private double normalizedAxisValue(ControlChairGamepadManager.Snapshot snapshot, int axis) {
        double raw = snapshot.axisValue(axis);
        if (isTriggerAxis(snapshot, axis)) {
            raw = (raw + 1.0) * 0.5;
        }
        if (Math.abs(raw) < 0.15) {
            return 0.0;
        }
        return raw;
    }

    private boolean isTriggerAxis(ControlChairGamepadManager.Snapshot snapshot, int axis) {
        return snapshot.mappedGamepad()
                && (axis == GLFW.GLFW_GAMEPAD_AXIS_LEFT_TRIGGER || axis == GLFW.GLFW_GAMEPAD_AXIS_RIGHT_TRIGGER);
    }

    private int totalPages() {
        return Math.max(1, (entryCount() + ROWS_PER_PAGE - 1) / ROWS_PER_PAGE);
    }

    private int entryCount() {
        return category == Category.AXES ? AxisDirection.VALUES.length : ControlChairButton.VALUES.length;
    }

    private Component primaryHeader() {
        return category == Category.AXES ? Component.literal("Axis") : Component.literal("Button A");
    }

    private Component secondaryHeader() {
        return category == Category.AXES ? Component.literal("Button") : Component.literal("Button B");
    }

    private Component sensitivityHeader() {
        return category == Category.AXES ? Component.literal("Gain") : Component.empty();
    }

    private String pageLabel() {
        return "Page " + (page + 1) + "/" + totalPages();
    }

    private Component footerText() {
        if (captureTarget != null) {
            if (captureTarget.acceptsAnalog()) {
                return Component.literal("Listening for axis input: move mouse or gamepad, Esc cancels");
            }
            return Component.literal("Listening for button input: key, mouse, or gamepad, Esc cancels");
        }

        ControlChairGamepadManager.Snapshot snapshot = ControlChairGamepadManager.snapshot();
        if (snapshot.connected()) {
            String name = snapshot.name() == null ? "Gamepad Connected" : "Gamepad: " + snapshot.name();
            return Component.literal(name);
        }

        return Component.literal("Axis gain is shared by +/- rows and applies to the final output");
    }

    private Component digitalBindingLabel(DigitalBinding binding) {
        if (!binding.isBound()) {
            return Component.literal("None");
        }

        return switch (binding.type()) {
            case NONE -> Component.literal("None");
            case KEYBOARD_KEY -> InputConstants.Type.KEYSYM.getOrCreate(binding.code()).getDisplayName();
            case MOUSE_BUTTON -> Component.literal(mouseButtonLabel(binding.code()));
            case GAMEPAD_BUTTON -> Component.literal(ControlChairGamepadManager.buttonLabel(binding.code()));
        };
    }

    private Component analogBindingLabel(AnalogBinding binding) {
        if (!binding.isBound()) {
            return Component.literal("None");
        }

        return switch (binding.type()) {
            case NONE -> Component.literal("None");
            case GAMEPAD_AXIS_POSITIVE -> Component.literal(ControlChairGamepadManager.axisLabel(binding.code()) + " +");
            case GAMEPAD_AXIS_NEGATIVE -> Component.literal(ControlChairGamepadManager.axisLabel(binding.code()) + " -");
            case MOUSE_YAW_POSITIVE -> Component.literal("Mouse Yaw +");
            case MOUSE_YAW_NEGATIVE -> Component.literal("Mouse Yaw -");
            case MOUSE_PITCH_POSITIVE -> Component.literal("Mouse Pitch +");
            case MOUSE_PITCH_NEGATIVE -> Component.literal("Mouse Pitch -");
        };
    }

    private Component trimText(Component component, int maxWidth) {
        String text = component.getString();
        if (font.width(text) <= maxWidth) {
            return Component.literal(text);
        }

        String trimmed = font.plainSubstrByWidth(text, Math.max(0, maxWidth - font.width("...")));
        return Component.literal(trimmed + "...");
    }

    private Component styled(String text, int color) {
        return Component.literal(text).withStyle(style -> style.withColor(color));
    }

    private Component styled(Component component, int color) {
        return Component.literal(component.getString()).withStyle(style -> style.withColor(color));
    }

    private String mouseButtonLabel(int button) {
        return switch (button) {
            case GLFW.GLFW_MOUSE_BUTTON_LEFT -> "Mouse L";
            case GLFW.GLFW_MOUSE_BUTTON_RIGHT -> "Mouse R";
            case GLFW.GLFW_MOUSE_BUTTON_MIDDLE -> "Mouse M";
            default -> "Mouse " + button;
        };
    }

    private String axisRowLabel(AxisDirection direction) {
        return switch (direction) {
            case YAW_POS -> "Yaw+";
            case YAW_NEG -> "Yaw-";
            case PITCH_POS -> "Pit+";
            case PITCH_NEG -> "Pit-";
            case ROLL_POS -> "Rol+";
            case ROLL_NEG -> "Rol-";
            case THRUST_POS -> "Thr+";
            case THRUST_NEG -> "Thr-";
        };
    }

    private String buttonRowLabel(ControlChairButton button) {
        return "B" + button.ordinal();
    }

    private static double sensitivityToSlider(double sensitivity) {
        double clamped = ControlChairBindingsProfile.clampSensitivity(sensitivity);
        return (clamped - ControlChairBindingsProfile.MIN_SENSITIVITY)
                / (ControlChairBindingsProfile.MAX_SENSITIVITY - ControlChairBindingsProfile.MIN_SENSITIVITY);
    }

    private static double sliderToSensitivity(double sliderValue) {
        return ControlChairBindingsProfile.MIN_SENSITIVITY
                + sliderValue * (ControlChairBindingsProfile.MAX_SENSITIVITY - ControlChairBindingsProfile.MIN_SENSITIVITY);
    }

    private static double snapSensitivity(double sensitivity) {
        double snapped = Math.round(ControlChairBindingsProfile.clampSensitivity(sensitivity) * 10.0) / 10.0;
        return ControlChairBindingsProfile.clampSensitivity(snapped);
    }

    private void renderPanelBackground(GuiGraphics graphics) {
        int left = guiLeft;
        int top = guiTop;
        int right = guiLeft + windowWidth;
        int bottom = guiTop + windowHeight;

        graphics.fill(left, top, right, bottom, BORDER_COLOR);
        graphics.fillGradient(left + 1, top + 1, right - 1, bottom - 1, PANEL_TOP_COLOR, PANEL_BOTTOM_COLOR);
        graphics.fillGradient(left + 1, top + 1, right - 1, top + 30, TITLE_BAR_TOP, TITLE_BAR_BOTTOM);
        graphics.fill(left + 1, bottom - 6, right - 1, bottom - 1, ACCENT_COLOR);

        int contentLeft = contentLeft() - 6;
        int contentRight = contentLeft() + contentWidth() + 6;
        graphics.fill(contentLeft, guiTop + HEADER_Y - 6, contentRight, guiTop + FOOTER_Y - 10, SECTION_COLOR);
        graphics.hLine(left + 1, right - 2, top + 30, DIVIDER_COLOR);
        graphics.hLine(contentLeft, contentRight, guiTop + HEADER_Y - 8, DIVIDER_COLOR);
        graphics.hLine(contentLeft, contentRight, guiTop + FOOTER_Y - 8, DIVIDER_COLOR);
    }

    private void renderBindingButtonText(GuiGraphics graphics) {
        for (RowContext context : rows) {
            if (!context.visible) {
                continue;
            }

            renderButtonText(graphics, context.primaryButton, context.primaryLabel);
            renderButtonText(graphics, context.secondaryButton, context.secondaryLabel);
        }
    }

    private void renderButtonText(GuiGraphics graphics, Button button, Component text) {
        if (!button.visible) {
            return;
        }

        String label = text.getString();
        if (label.isEmpty()) {
            return;
        }

        int left = button.getX() + BINDING_TEXT_PADDING;
        int right = button.getX() + button.getWidth() - BINDING_TEXT_PADDING;
        int availableWidth = right - left;
        if (availableWidth <= 0) {
            return;
        }

        int textWidth = font.width(label);
        int drawY = button.getY() + (button.getHeight() - font.lineHeight) / 2;
        int drawX;
        if (textWidth <= availableWidth) {
            drawX = left + (availableWidth - textWidth) / 2;
        } else {
            int overflow = textWidth - availableWidth;
            int offset = button.isHoveredOrFocused() ? marqueeOffset(overflow) : 0;
            drawX = left - offset;
        }

        int color = button.active ? 0xFFFFFF : 0xA0A0A0;
        graphics.enableScissor(left, button.getY() + 2, right, button.getY() + button.getHeight() - 2);
        graphics.drawString(font, label, drawX, drawY, color, false);
        graphics.disableScissor();
    }

    private int marqueeOffset(int overflow) {
        if (overflow <= 0) {
            return 0;
        }

        double travelDuration = overflow * 1000.0 / MARQUEE_PIXELS_PER_SECOND;
        double cycle = MARQUEE_HOLD_MS + travelDuration + MARQUEE_HOLD_MS + travelDuration;
        double phase = Util.getMillis() % cycle;
        if (phase < MARQUEE_HOLD_MS) {
            return 0;
        }

        phase -= MARQUEE_HOLD_MS;
        if (phase < travelDuration) {
            return (int) Math.round(overflow * (phase / travelDuration));
        }

        phase -= travelDuration;
        if (phase < MARQUEE_HOLD_MS) {
            return overflow;
        }

        phase -= MARQUEE_HOLD_MS;
        return overflow - (int) Math.round(overflow * (phase / travelDuration));
    }

    private enum Category {
        AXES,
        BUTTONS
    }

    private enum Slot {
        PRIMARY,
        CLEAR_PRIMARY,
        SECONDARY,
        CLEAR_SECONDARY
    }

    private record CaptureTarget(
            CaptureSlotType slotType,
            @Nullable AxisDirection axisDirection,
            @Nullable ControlChairButton button
    ) {
        private static CaptureTarget axisAnalog(AxisDirection direction) {
            return new CaptureTarget(CaptureSlotType.AXIS_ANALOG, direction, null);
        }

        private static CaptureTarget axisDigital(AxisDirection direction) {
            return new CaptureTarget(CaptureSlotType.AXIS_DIGITAL, direction, null);
        }

        private static CaptureTarget buttonPrimary(ControlChairButton button) {
            return new CaptureTarget(CaptureSlotType.BUTTON_PRIMARY, null, button);
        }

        private static CaptureTarget buttonSecondary(ControlChairButton button) {
            return new CaptureTarget(CaptureSlotType.BUTTON_SECONDARY, null, button);
        }

        private boolean acceptsDigital() {
            return slotType != CaptureSlotType.AXIS_ANALOG;
        }

        private boolean acceptsAnalog() {
            return slotType == CaptureSlotType.AXIS_ANALOG;
        }
    }

    private enum CaptureSlotType {
        AXIS_ANALOG,
        AXIS_DIGITAL,
        BUTTON_PRIMARY,
        BUTTON_SECONDARY
    }

    private final class SensitivitySlider extends AbstractSliderButton {
        private @Nullable ControlChairAxis axis = null;
        private boolean suppressWrite = false;

        private SensitivitySlider() {
            super(0, 0, SENSITIVITY_WIDTH, BUTTON_HEIGHT, Component.empty(), sensitivityToSlider(ControlChairBindingsProfile.DEFAULT_SENSITIVITY));
            updateMessage();
        }

        private void bind(ControlChairAxis axis, double sensitivity) {
            this.axis = axis;
            setSensitivityInternal(sensitivity);
        }

        private void clearBinding() {
            this.axis = null;
        }

        private void setSensitivityInternal(double sensitivity) {
            suppressWrite = true;
            this.value = sensitivityToSlider(snapSensitivity(sensitivity));
            updateMessage();
            suppressWrite = false;
        }

        private double currentSensitivity() {
            return snapSensitivity(sliderToSensitivity(value));
        }

        @Override
        protected void updateMessage() {
            setMessage(Component.literal(String.format(Locale.ROOT, "%.1f", currentSensitivity())));
        }

        @Override
        public void renderWidget(GuiGraphics graphics, int mouseX, int mouseY, float partialTicks) {
            int left = getX();
            int top = getY();
            int width = getWidth();
            int height = getHeight();

            int textColor = active ? SLIDER_TEXT_COLOR : SLIDER_TEXT_DISABLED_COLOR;
            int trackLeft = left + 1;
            int trackRight = left + width - 1;
            int trackY = top + height - SLIDER_TRACK_HEIGHT - 1;
            int handleTravel = Math.max(0, (trackRight - trackLeft) - SLIDER_HANDLE_WIDTH);
            int handleX = trackLeft + (int) Math.round(handleTravel * value);
            int handleY = trackY - ((SLIDER_HANDLE_HEIGHT - SLIDER_TRACK_HEIGHT) / 2);
            int fillRight = trackLeft + (int) Math.round((trackRight - trackLeft) * value);

            graphics.drawCenteredString(font, getMessage(), left + width / 2, top, textColor);
            graphics.fill(trackLeft, trackY, trackRight, trackY + SLIDER_TRACK_HEIGHT, SLIDER_TRACK_COLOR);
            if (fillRight > trackLeft) {
                graphics.fill(trackLeft, trackY, fillRight, trackY + SLIDER_TRACK_HEIGHT,
                        isHoveredOrFocused() ? SLIDER_TRACK_FILL_HOVER_COLOR : SLIDER_TRACK_FILL_COLOR);
            }
            graphics.fill(handleX, handleY, handleX + SLIDER_HANDLE_WIDTH, handleY + SLIDER_HANDLE_HEIGHT,
                    active ? SLIDER_HANDLE_COLOR : SLIDER_HANDLE_DISABLED_COLOR);
        }

        @Override
        protected void applyValue() {
            double snapped = currentSensitivity();
            this.value = sensitivityToSlider(snapped);
            updateMessage();
            if (!suppressWrite && axis != null) {
                workingCopy.setAxisSensitivity(axis, snapped);
            }
        }
    }

    private static class RowContext {
        private boolean visible = false;
        private boolean showSensitivity = false;
        private @Nullable ControlChairAxis sensitivityAxis = null;
        private double sensitivity = ControlChairBindingsProfile.DEFAULT_SENSITIVITY;
        private Component label = Component.empty();
        private Component primaryLabel = Component.empty();
        private Component secondaryLabel = Component.empty();
        private Runnable primaryAction = () -> {};
        private Runnable clearPrimaryAction = () -> {};
        private Runnable secondaryAction = () -> {};
        private Runnable clearSecondaryAction = () -> {};
        private GridLayout layout;
        private FormattedLabel labelWidget;
        private Button primaryButton;
        private Button clearPrimaryButton;
        private Button secondaryButton;
        private Button clearSecondaryButton;
        private SensitivitySlider sensitivitySlider;

        private void clear() {
            visible = false;
            showSensitivity = false;
            sensitivityAxis = null;
            sensitivity = ControlChairBindingsProfile.DEFAULT_SENSITIVITY;
            label = Component.empty();
            primaryLabel = Component.empty();
            secondaryLabel = Component.empty();
            primaryAction = () -> {};
            clearPrimaryAction = () -> {};
            secondaryAction = () -> {};
            clearSecondaryAction = () -> {};
        }
    }
}
