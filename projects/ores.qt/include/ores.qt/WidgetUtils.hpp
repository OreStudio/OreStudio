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

#include <QComboBox>
#include <QListView>
#include <QSize>
#include <QShowEvent>
#include <QTimer>
#include <QWidget>

namespace ores::qt {

/**
 * @brief A QListView that caps the combo box popup height.
 *
 * Qt's popup sizing on Linux can ignore setMaxVisibleItems when items carry
 * icons, because the popup container may be sized from sizeHintForRow() *
 * itemCount rather than from sizeHint(). Two complementary mechanisms are
 * used here:
 *
 *  1. sizeHint() – caps the natural hint for platforms that query it.
 *  2. showEvent() – posts a deferred resize of the popup frame that fires
 *     after QComboBox::showPopup() has fully completed its own sizing, so
 *     it is effective even when the hint is bypassed.
 */
class BoundedListView : public QListView {
public:
    static constexpr int max_popup_height = 250;

    explicit BoundedListView(QWidget* parent = nullptr) : QListView(parent) {}

    QSize sizeHint() const override {
        const auto s = QListView::sizeHint();
        return { s.width(), std::min(s.height(), max_popup_height) };
    }

protected:
    void showEvent(QShowEvent* event) override {
        QListView::showEvent(event);
        // Post to the event queue so this runs after showPopup() completes.
        // QTimer::singleShot cancels automatically if `this` is destroyed.
        QTimer::singleShot(0, this, [this]() {
            if (QWidget* popup = parentWidget()) {
                if (popup->height() > max_popup_height)
                    popup->resize(popup->width(), max_popup_height);
            }
        });
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
            // Replace the default popup view so both sizeHint capping and the
            // deferred showEvent resize are active for this combo.
            combo->setView(new BoundedListView(combo));
        }
    }
};

}

#endif
