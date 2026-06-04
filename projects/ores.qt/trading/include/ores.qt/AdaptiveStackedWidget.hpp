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
#ifndef ORES_QT_ADAPTIVE_STACKED_WIDGET_HPP
#define ORES_QT_ADAPTIVE_STACKED_WIDGET_HPP

#include <QStackedWidget>

/**
 * @brief QStackedWidget that reports the current page's size hints.
 *
 * The standard QStackedWidget::sizeHint() returns the maximum of all
 * pages, which causes the containing dialog to be as tall as the largest
 * form even when a compact form is shown. This subclass overrides
 * sizeHint() and minimumSizeHint() to return the current page's values so
 * that layouts and scroll areas can size themselves correctly.
 *
 * Must be in the global namespace so uic generates unqualified class names.
 */
class AdaptiveStackedWidget : public QStackedWidget {
public:
    explicit AdaptiveStackedWidget(QWidget* parent = nullptr)
        : QStackedWidget(parent) {
        connect(this, &QStackedWidget::currentChanged, this, [this](int) { updateGeometry(); });
    }

    QSize sizeHint() const override {
        if (const auto* w = currentWidget())
            return w->sizeHint();
        return QStackedWidget::sizeHint();
    }

    QSize minimumSizeHint() const override {
        if (const auto* w = currentWidget())
            return w->minimumSizeHint();
        return QStackedWidget::minimumSizeHint();
    }
};

#endif
