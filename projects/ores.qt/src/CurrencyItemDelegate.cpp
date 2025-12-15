/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2024 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.qt/CurrencyItemDelegate.hpp"

#include <QPainter>
#include <QApplication>
#include <QStyleOptionViewItem>

namespace ores::qt {

CurrencyItemDelegate::CurrencyItemDelegate(QObject* parent)
    : QStyledItemDelegate(parent) {
    monospaceFont_ = QFont("Fira Code"); // Use the same monospace font as in QSS
    monospaceFont_.setPointSize(10); // Consistent with QWidget default
}

void CurrencyItemDelegate::paint(QPainter* painter, const QStyleOptionViewItem& option,
                                 const QModelIndex& index) const {
    QStyleOptionViewItem opt = option;
    initStyleOption(&opt, index);

    // Check if model provides a custom foreground color (e.g., recency coloring)
    QVariant fgData = index.data(Qt::ForegroundRole);
    if (fgData.isValid()) {
        opt.palette.setColor(QPalette::Text, fgData.value<QColor>());
        opt.palette.setColor(QPalette::HighlightedText, fgData.value<QColor>());
    }

    // Apply monospace font and alignment based on column
    // Column indices match ClientCurrencyModel::Column enum
    switch (index.column()) {
        case 0: // CurrencyName
            opt.displayAlignment = Qt::AlignLeft | Qt::AlignVCenter;
            break;
        case 1: // IsoCode
            opt.font = monospaceFont_;
            opt.font.setBold(true);
            opt.displayAlignment = Qt::AlignCenter;
            break;
        case 2: // Version
            opt.font = monospaceFont_;
            opt.displayAlignment = Qt::AlignCenter;
            break;
        case 3: // NumericCode
            opt.font = monospaceFont_;
            opt.displayAlignment = Qt::AlignCenter;
            break;
        case 4: // Symbol
            opt.font = monospaceFont_;
            opt.displayAlignment = Qt::AlignCenter;
            break;
        case 5: // FractionSymbol
            opt.font = monospaceFont_;
            opt.displayAlignment = Qt::AlignCenter;
            break;
        case 6: // FractionsPerUnit
            opt.font = monospaceFont_;
            opt.displayAlignment = Qt::AlignRight | Qt::AlignVCenter;
            break;
        case 7: // RoundingType
            opt.displayAlignment = Qt::AlignLeft | Qt::AlignVCenter;
            break;
        case 8: // RoundingPrecision
            opt.font = monospaceFont_;
            opt.displayAlignment = Qt::AlignRight | Qt::AlignVCenter;
            break;
        case 9: // Format
            opt.displayAlignment = Qt::AlignLeft | Qt::AlignVCenter;
            break;
        case 10: // CurrencyType
            opt.displayAlignment = Qt::AlignLeft | Qt::AlignVCenter;
            break;
        case 11: // recorded_at
            opt.font = monospaceFont_;
            opt.displayAlignment = Qt::AlignLeft | Qt::AlignVCenter;
            break;
        default:
            break;
    }

    // Draw the item using the modified options
    QApplication::style()->drawControl(QStyle::CE_ItemViewItem, &opt, painter);
}

}
