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
#include <QListView>
#include <QPointer>
#include <QScreen>
#include <QShowEvent>
#include <QSize>
#include <QTimer>
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
        : QListView(combo), combo_(combo) {}

    QSize sizeHint() const override {
        const auto s = QListView::sizeHint();
        return { s.width(), std::min(s.height(), max_popup_height) };
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
            const QRect screenRect = screen
                ? screen->availableGeometry()
                : QRect(0, 0, 9999, 9999);

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
};

}

#endif
