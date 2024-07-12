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
#include "ores.core/ore/db/currency_table.hpp"
#include "ores.qt/CurrencyModel.hpp"

namespace ores::qt {

CurrencyModel::CurrencyModel(QObject* parent)
    : QAbstractTableModel(parent) {

    ores::core::ore::db::currency_table table;
    currencies_ = table.read();
}

int CurrencyModel::rowCount(const QModelIndex& /*parent*/) const {
    return currencies_.size();
}

int CurrencyModel::columnCount(const QModelIndex& /*parent*/) const {
    return 10;
}

QVariant CurrencyModel::data(const QModelIndex& index, int role) const {
    if (role == Qt::DisplayRole) {
        auto currency = currencies_[index.row()];
        switch(index.column()) {
        case 0: return QString::fromStdString(currency.name());
        case 1: return QString::fromStdString(currency.iso_code());
        case 2: return QString::number(currency.numeric_code());
        case 3: return QString::fromStdString(currency.symbol());
        case 4: return QString::fromStdString(currency.fraction_symbol());
        case 5: return QString::number(currency.fractions_per_unit());
        case 6: return QString::fromStdString(currency.rounding_type());
        case 7: return QString::number(currency.rounding_precision());
        case 8: return QString::fromStdString(currency.format());
        case 9: return QString::fromStdString(currency.currency_type());
        }
    }

    return {};
}

}
