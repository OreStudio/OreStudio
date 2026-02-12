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
#ifndef ORES_QT_DELEGATE_PAINT_UTILS_HPP
#define ORES_QT_DELEGATE_PAINT_UTILS_HPP

#include <QFont>
#include <QRect>
#include <QColor>
#include <QString>
#include <QPainter>
#include <QModelIndex>
#include <QStyleOptionViewItem>

namespace ores::qt {

struct DelegatePaintUtils {
    /**
     * @brief Paints a centered icon from DecorationRole data.
     *
     * Extracts the QIcon from the model's DecorationRole, scales it to fit
     * within the cell (with padding), and draws it centered. Used for flag
     * icon columns.
     */
    static void paint_centered_icon(QPainter* painter,
        const QStyleOptionViewItem& option, const QModelIndex& index,
        int padding = 4);

    /**
     * @brief Applies ForegroundRole color to both Text and HighlightedText.
     *
     * If the model provides a custom foreground color (e.g., recency coloring),
     * applies it to both palette roles so it works in both normal and selected
     * states.
     */
    static void apply_foreground_role(QStyleOptionViewItem& opt,
        const QModelIndex& index);

    /**
     * @brief Draws a centered pill-shaped badge with text.
     *
     * Renders a rounded rectangle badge centered in the given rect, with the
     * specified background color, text color, and font.
     */
    static void draw_centered_badge(QPainter* painter, const QRect& rect,
        const QString& text, const QColor& bg, const QColor& fg,
        const QFont& font);

    /**
     * @brief Draws a left-aligned inline badge and advances the rect.
     *
     * Renders a pill badge at the left edge of rect, then moves rect.left()
     * past the badge plus spacing, ready for the next badge.
     */
    static void draw_inline_badge(QPainter* painter, QRect& rect,
        const QString& text, const QColor& bg, const QColor& fg,
        const QFont& font, int padding = 4, int spacing = 3);
};

}

#endif
