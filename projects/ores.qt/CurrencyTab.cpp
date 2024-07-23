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
#include <QtCore/QVariant>
#include <QtWidgets/QApplication>
#include <QtWidgets/QHeaderView>
#include <QtWidgets/QTableView>
#include <QtWidgets/QWidget>
#include "ores.qt/CurrencyTab.hpp"

namespace ores::qt {

CurrencyTab::CurrencyTab(QWidget* parent) : QWidget(parent) {

    verticalLayout_ = new QVBoxLayout(this);
    currencyTableView_ = new QTableView(this);
    verticalLayout_->addWidget(currencyTableView_);

    currencyTableView_->setObjectName("currencyTableView");
    currencyTableView_->setGeometry(QRect(0, 0, 1071, 641));
    currencyTableView_->setAlternatingRowColors(true);
    currencyTableView_->setSelectionMode(QAbstractItemView::SingleSelection);
    currencyTableView_->setSelectionBehavior(QAbstractItemView::SelectRows);
    currencyTableView_->setModel(&currencyModel_);
    currencyTableView_->resizeRowsToContents();

    QHeaderView* verticalHeader(currencyTableView_->verticalHeader());
    QHeaderView* horizontalHeader(currencyTableView_->horizontalHeader());
    verticalHeader->setSectionResizeMode(QHeaderView::ResizeToContents);
    horizontalHeader->setSectionResizeMode(QHeaderView::ResizeToContents);
}

}
