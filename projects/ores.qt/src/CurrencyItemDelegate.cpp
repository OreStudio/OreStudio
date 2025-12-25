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
#include "ores.qt/ClientCurrencyModel.hpp"

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
    using Column = ClientCurrencyModel::Column;
    switch (index.column()) {
        case Column::Flag:
            opt.decorationAlignment = Qt::AlignCenter;
            opt.decorationPosition = QStyleOptionViewItem::Top;
            break;
        case Column::CurrencyName:
            opt.displayAlignment = Qt::AlignLeft | Qt::AlignVCenter;
            break;
        case Column::IsoCode:
            opt.font = monospaceFont_;
            opt.font.setBold(true);
            opt.displayAlignment = Qt::AlignCenter;
            break;
        case Column::Version:
            opt.font = monospaceFont_;
            opt.displayAlignment = Qt::AlignCenter;
            break;
        case Column::NumericCode:
            opt.font = monospaceFont_;
            opt.displayAlignment = Qt::AlignCenter;
            break;
        case Column::Symbol:
            opt.font = monospaceFont_;
            opt.displayAlignment = Qt::AlignCenter;
            break;
        case Column::FractionSymbol:
            opt.font = monospaceFont_;
            opt.displayAlignment = Qt::AlignCenter;
            break;
        case Column::FractionsPerUnit:
            opt.font = monospaceFont_;
            opt.displayAlignment = Qt::AlignRight | Qt::AlignVCenter;
            break;
        case Column::RoundingType:
            opt.displayAlignment = Qt::AlignLeft | Qt::AlignVCenter;
            break;
        case Column::RoundingPrecision:
            opt.font = monospaceFont_;
            opt.displayAlignment = Qt::AlignRight | Qt::AlignVCenter;
            break;
        case Column::Format:
            opt.displayAlignment = Qt::AlignLeft | Qt::AlignVCenter;
            break;
        case Column::CurrencyType:
            opt.displayAlignment = Qt::AlignLeft | Qt::AlignVCenter;
            break;
        case Column::RecordedBy:
            opt.displayAlignment = Qt::AlignLeft | Qt::AlignVCenter;
            break;
        case Column::RecordedAt:
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
