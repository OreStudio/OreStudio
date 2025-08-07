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
#include "ores.qt/CurrencyModel.hpp"

namespace ores::qt {

CurrencyModel::CurrencyModel(QObject* parent, const QSqlDatabase& db)
    : QSqlRelationalTableModel(parent, db) {

    setTable("oresdb.currencies");
    setRelation(10, QSqlRelation("Currencies", "iso_code", "name"));

    setHeaderData(0, Qt::Horizontal, QObject::tr("Currency Name"));
    setHeaderData(1, Qt::Horizontal, QObject::tr("ISO Code"));
    setHeaderData(2, Qt::Horizontal, QObject::tr("Numeric Code"));
    setHeaderData(3, Qt::Horizontal, QObject::tr("Symbol"));
    setHeaderData(4, Qt::Horizontal, QObject::tr("Frac. Symbol"));
    setHeaderData(5, Qt::Horizontal, QObject::tr("Frac. per unit"));
    setHeaderData(6, Qt::Horizontal, QObject::tr("Rounding type"));
    setHeaderData(7, Qt::Horizontal, QObject::tr("Rounding precision"));
    setHeaderData(8, Qt::Horizontal, QObject::tr("Format"));
    setHeaderData(9, Qt::Horizontal, QObject::tr("Currency Type"));
}

}
