/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_QT_WIDGET_UTILS_HPP
#define ORES_QT_WIDGET_UTILS_HPP

#include <QApplication>
#include <QComboBox>
#include <QEvent>
#include <QListView>
#include <QPointer>
#include <QScreen>
#include <QShowEvent>
#include <QSize>
#include <QTimer>
#include <QVariant>
#include <QWidget>

namespace ores::qt {

/**
 * @brief A QListView that caps the combo box popup height and repositions it.
 *
 * On Linux, Qt can compute the popup size from sizeHintForRow() * itemCount,
 * ignoring setMaxVisibleItems(), and position the popup above the combo when
 * it would overflow the screen. A deferred showEvent() resize then corrects
 * both the height and the vertical position relative to the combo box.
 *
 * sizeHint() is also overridden as a first-pass hint for platforms that do
 * query it before positioning.
 */
class BoundedListView : public QListView {
public:
    static constexpr int max_popup_height = 250;

    explicit BoundedListView(QComboBox* combo)
        : QListView(combo)
        , combo_(combo) {}

    QSize sizeHint() const override {
        const auto s = QListView::sizeHint();
        return {s.width(), std::min(s.height(), max_popup_height)};
    }

protected:
    void showEvent(QShowEvent* event) override {
        QListView::showEvent(event);
        // Post so this runs after QComboBox::showPopup() finishes sizing and
        // positioning. We then correct both if needed.
        QTimer::singleShot(0, this, [this]() {
            QWidget* popup = parentWidget();
            if (!popup || !combo_ || popup->height() <= max_popup_height)
                return;

            // Determine where the combo box is on screen.
            const QPoint comboPos = combo_->mapToGlobal(QPoint(0, 0));
            const QRect comboRect(comboPos, combo_->size());

            // Get the available screen geometry.
            const QScreen* screen = QApplication::screenAt(comboPos);
            const QRect screenRect = screen ? screen->availableGeometry() : QRect(0, 0, 9999, 9999);

            // Resize first, then reposition: prefer opening below the combo,
            // fall back to above if there is not enough room.
            popup->resize(popup->width(), max_popup_height);
            int y = comboRect.bottom() + 1;
            if (y + max_popup_height > screenRect.bottom())
                y = comboRect.top() - max_popup_height;
            popup->move(popup->x(), y);
        });
    }

private:
    QPointer<QComboBox> combo_;
};

/**
 * @brief Swallows the interaction events that let a user change a
 * QComboBox's selection, while leaving the widget fully enabled (normal
 * colour, icon still visible) — unlike setEnabled(false), which grays the
 * whole widget out and typically fades its icon too. Installed/removed via
 * set_combo_locked() below.
 */
class ComboLockFilter final : public QObject {
public:
    using QObject::QObject;

protected:
    bool eventFilter(QObject* watched, QEvent* event) override {
        switch (event->type()) {
            case QEvent::MouseButtonPress:
            case QEvent::MouseButtonRelease:
            case QEvent::MouseButtonDblClick:
            case QEvent::Wheel:
            case QEvent::KeyPress:
                return true; // swallow: block the interaction, keep the widget visually normal
            default:
                return QObject::eventFilter(watched, event);
        }
    }
};

/**
 * @brief Utility functions for common widget configuration.
 */
struct WidgetUtils {

    /**
     * @brief Apply standard configuration to all combo boxes in a widget.
     *
     * Finds every QComboBox that is a descendant of @p parent and applies
     * project-wide defaults. Call this once after setupUi() or after
     * programmatic UI construction.
     *
     * @param parent The widget whose combo box children to configure.
     */
    static void setupComboBoxes(QWidget* parent) {
        for (auto* combo : parent->findChildren<QComboBox*>()) {
            combo->setMaxVisibleItems(10);
            combo->setView(new BoundedListView(combo));
        }
    }

    /**
     * @brief Lock or unlock a combo box's selection without graying it out.
     *
     * For an identity-defining field (e.g. currency_pair's base/quote
     * currency once created) that should read as normal, fully-visible
     * content — flag icon included — rather than the faded
     * setEnabled(false) look. Safe to call repeatedly; only one filter is
     * ever installed per combo.
     *
     * @param combo  The combo box to lock/unlock (no-op if null).
     * @param locked True to block interaction, false to restore it.
     */
    static void set_combo_locked(QComboBox* combo, bool locked) {
        if (!combo)
            return;
        // Always visually enabled — locking is entirely via the event
        // filter below, so this also undoes a prior setEnabled(false)
        // from generated code that ran before this call (the paste-block
        // seams that call this always run after the generated per-field
        // lock logic).
        combo->setEnabled(true);
        static const char* filter_property = "ores_combo_lock_filter";
        auto* existing = combo->property(filter_property).value<QObject*>();
        if (locked) {
            if (existing)
                return;
            auto* filter = new ComboLockFilter(combo);
            combo->installEventFilter(filter);
            combo->setProperty(filter_property, QVariant::fromValue(filter));
            combo->setFocusPolicy(Qt::NoFocus);
        } else {
            if (existing) {
                combo->removeEventFilter(existing);
                existing->deleteLater();
                combo->setProperty(filter_property, QVariant());
            }
            combo->setFocusPolicy(Qt::WheelFocus);
        }
    }

    /**
     * @brief Disable a combo box for a field that's genuinely not
     * applicable right now (as opposed to set_combo_locked(), for a field
     * that's still relevant but not editable) — grays it out via the
     * normal setEnabled(false) look *and* clears any stale selection, so
     * a disabled combo never shows a leftover value the user might read
     * as still in effect.
     *
     * @param combo        The combo box (no-op if null).
     * @param unavailable  True to disable and clear; false to re-enable
     * (leaves the selection empty — the caller repopulates as needed).
     */
    static void set_combo_unavailable(QComboBox* combo, bool unavailable) {
        if (!combo)
            return;
        combo->setEnabled(!unavailable);
        if (unavailable)
            combo->setCurrentIndex(-1);
    }
};

}

#endif
