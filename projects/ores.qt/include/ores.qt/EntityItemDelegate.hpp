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
#ifndef ORES_QT_ENTITY_ITEM_DELEGATE_HPP
#define ORES_QT_ENTITY_ITEM_DELEGATE_HPP

#include <functional>
#include <vector>
#include <QColor>
#include <QFont>
#include <QStyledItemDelegate>

namespace ores::qt {

/**
 * @brief Column rendering style for the data-driven EntityItemDelegate.
 */
enum class column_style {
    text_left,          ///< Proportional font, left-aligned (default).
    text_center,        ///< Proportional font, centered.
    mono_left,          ///< Monospace, left-aligned.
    mono_center,        ///< Monospace, centered.
    mono_bold_left,     ///< Monospace bold, left-aligned.
    mono_right,         ///< Monospace, right-aligned.
    mono_bold_center,   ///< Monospace bold, centered.
    icon_centered,      ///< Centered DecorationRole icon (flag columns).
    badge_centered      ///< Coloured badge pill, centered.
};

struct badge_color_pair {
    QColor background;
    QColor foreground;
};

/**
 * @brief Callback that resolves a display-text value to badge colours.
 */
using badge_color_resolver = std::function<badge_color_pair(const QString& value)>;

/**
 * @brief Data-driven item delegate configured by a per-column style vector.
 *
 * Replaces the need for per-entity delegate subclasses for simple column
 * formatting (fonts, alignment, icon centering, ForegroundRole).
 */
class EntityItemDelegate : public QStyledItemDelegate {
    Q_OBJECT

public:
    explicit EntityItemDelegate(std::vector<column_style> styles,
        QObject* parent = nullptr);

    void paint(QPainter* painter, const QStyleOptionViewItem& option,
        const QModelIndex& index) const override;

    void set_badge_color_resolver(badge_color_resolver resolver);

private:
    std::vector<column_style> styles_;
    QFont monospaceFont_;
    badge_color_resolver badge_resolver_;
};

}

#endif
