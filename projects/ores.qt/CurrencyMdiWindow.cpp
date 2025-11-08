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
#include "ores.qt/CurrencyMdiWindow.hpp"

namespace ores::qt {

using namespace ores::utility::log;

CurrencyMdiWindow::CurrencyMdiWindow(std::shared_ptr<comms::client> client, QWidget* parent)
    : QWidget(parent),
      currencyModel_(new ClientCurrencyModel(std::move(client), this)) {

    BOOST_LOG_SEV(lg(), info) << "Creating currency MDI window";

    verticalLayout_ = new QVBoxLayout(this);

    // Add table view
    currencyTableView_ = new QTableView(this);
    verticalLayout_->addWidget(currencyTableView_);

    currencyTableView_->setObjectName("currencyTableView");
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
            this, &CurrencyMdiWindow::onDataLoaded);
    connect(currencyModel_, &ClientCurrencyModel::loadError,
            this, &CurrencyMdiWindow::onLoadError);

    // Emit initial loading status
    emit statusChanged("Loading currencies...");

    // Trigger data loading
    currencyModel_->refresh();
}

void CurrencyMdiWindow::onDataLoaded() {
    const QString message = QString("Loaded %1 currencies")
                              .arg(currencyModel_->rowCount());
    emit statusChanged(message);
    BOOST_LOG_SEV(lg(), info) << "Currency data loaded successfully: "
                             << currencyModel_->rowCount() << " currencies";
}

void CurrencyMdiWindow::onLoadError(const QString& error_message) {
    emit errorOccurred(error_message);
    BOOST_LOG_SEV(lg(), error) << "Error loading currencies: "
                              << error_message.toStdString();

    QMessageBox::critical(this, "Load Error", error_message);
}

}
