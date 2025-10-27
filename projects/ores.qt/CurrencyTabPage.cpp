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
#include <QtWidgets/QWidget>
#include <QtWidgets/QHeaderView>
#include <QtWidgets/QTableView>
#include <QtWidgets/QApplication>
#include <QtWidgets/QMessageBox>
#include "ores.qt/CurrencyTabPage.hpp"

namespace ores::qt {

CurrencyTabPage::
CurrencyTabPage(std::shared_ptr<comms::client> client, QWidget* parent)
    : QWidget(parent),
      currencyModel_(new ClientCurrencyModel(std::move(client), this)) {

    verticalLayout_ = new QVBoxLayout(this);

    // Add status label at the top
    statusLabel_ = new QLabel("Loading currencies...", this);
    statusLabel_->setStyleSheet("QLabel { padding: 5px; color: #666; font-style: italic; }");
    verticalLayout_->addWidget(statusLabel_);

    // Add table view
    currencyTableView_ = new QTableView(this);
    verticalLayout_->addWidget(currencyTableView_);

    currencyTableView_->setObjectName("currencyTableView");
    currencyTableView_->setGeometry(QRect(0, 0, 1071, 641));
    currencyTableView_->setAlternatingRowColors(true);
    currencyTableView_->setSelectionMode(QAbstractItemView::SingleSelection);
    currencyTableView_->setSelectionBehavior(QAbstractItemView::SelectRows);
    currencyTableView_->resizeRowsToContents();

    // Set the model
    currencyTableView_->setModel(currencyModel_);

    // Configure headers
    QHeaderView* verticalHeader(currencyTableView_->verticalHeader());
    QHeaderView* horizontalHeader(currencyTableView_->horizontalHeader());
    verticalHeader->setSectionResizeMode(QHeaderView::ResizeToContents);
    horizontalHeader->setSectionResizeMode(QHeaderView::ResizeToContents);

    // Connect signals
    connect(currencyModel_, &ClientCurrencyModel::dataLoaded,
            this, &CurrencyTabPage::onDataLoaded);
    connect(currencyModel_, &ClientCurrencyModel::loadError,
            this, &CurrencyTabPage::onLoadError);

    // Trigger data loading
    currencyModel_->refresh();
}

void CurrencyTabPage::onDataLoaded() {
    statusLabel_->setText(QString("Loaded %1 currencies")
                              .arg(currencyModel_->rowCount()));
    statusLabel_->setStyleSheet("QLabel { padding: 5px; color: #0a0; }");
}

void CurrencyTabPage::onLoadError(const QString& error_message) {
    statusLabel_->setText("Error loading currencies");
    statusLabel_->setStyleSheet("QLabel { padding: 5px; color: #c00; }");

    QMessageBox::critical(this, "Load Error", error_message);
}

}
