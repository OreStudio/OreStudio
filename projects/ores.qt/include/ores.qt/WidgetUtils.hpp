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
#include <QWidget>

namespace ores::qt {

/**
 * @brief A QListView that caps its sizeHint height.
 *
 * Qt's combo popup sizing queries view()->sizeHint() before calling resize()
 * on the popup container. Capping that hint here — rather than setMaximumHeight
 * — ensures the popup frame itself is sized correctly, not just the content area.
 */
class BoundedListView : public QListView {
public:
    static constexpr int max_popup_height = 250;

    explicit BoundedListView(QWidget* parent = nullptr) : QListView(parent) {}

    QSize sizeHint() const override {
        const auto s = QListView::sizeHint();
        return { s.width(), std::min(s.height(), max_popup_height) };
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
            // Replace the default popup view with one that caps sizeHint().
            // setMaxVisibleItems alone is ignored on Linux when items carry
            // icons; the popup container is sized from the view's sizeHint(),
            // so overriding that is the reliable cross-platform fix.
            combo->setView(new BoundedListView(combo));
        }
    }
};

}

#endif
