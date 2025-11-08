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
#include <QtCore/QTimer>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <QtWidgets/QWidget>
#include <QtWidgets/QHeaderView>
#include <QtWidgets/QTableView>
#include <QtWidgets/QApplication>
#include "ores.qt/CurrencyMdiWindow.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
// #include "ores.qt/CurrencyEditDialog.hpp" // Removed
#include "ores.qt/CurrencyItemDelegate.hpp" // Include the new delegate header
#include "ores.risk/messaging/protocol.hpp"
#include "ores.comms/protocol/frame.hpp"

namespace ores::qt {

using namespace ores::utility::log;

CurrencyMdiWindow::CurrencyMdiWindow(std::shared_ptr<comms::client> client, QWidget* parent)
    : QWidget(parent),
      currencyModel_(new ClientCurrencyModel(client, this)),
      client_(std::move(client)) {

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

    // Set the custom item delegate
    currencyTableView_->setItemDelegate(new CurrencyItemDelegate(this));

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
    connect(currencyTableView_, &QTableView::doubleClicked,
            this, &CurrencyMdiWindow::onRowDoubleClicked);
    connect(currencyTableView_->selectionModel(), &QItemSelectionModel::selectionChanged,
            this, &CurrencyMdiWindow::onSelectionChanged);

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

    MessageBoxHelper::critical(this, "Load Error", error_message);
}

void CurrencyMdiWindow::onRowDoubleClicked(const QModelIndex& index) {
    if (!index.isValid()) {
        return;
    }

    const auto* currency = currencyModel_->getCurrency(index.row());
    if (!currency) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to get currency for row: "
                                 << index.row();
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Emitting showCurrencyDetails for currency: "
                             << currency->iso_code;
    emit showCurrencyDetails(*currency);
}

void CurrencyMdiWindow::onSelectionChanged() {
    const bool has_selection = currencyTableView_->selectionModel()->hasSelection();
    emit selectionChanged(has_selection);
}

void CurrencyMdiWindow::editSelected() {
    const auto selected = currencyTableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Edit requested but no row selected";
        return;
    }

    // Use the first selected row (single selection mode)
    onRowDoubleClicked(selected.first());
}

void CurrencyMdiWindow::deleteSelected() {
    const auto selected = currencyTableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Delete requested but no row selected";
        return;
    }

    const auto index = selected.first();
    const auto* currency = currencyModel_->getCurrency(index.row());
    if (!currency) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to get currency for row: "
                                 << index.row();
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Delete request for currency: "
                             << currency->iso_code;

    // Confirm deletion
    auto reply = MessageBoxHelper::question(this, "Delete Currency",
        QString("Are you sure you want to delete currency '%1' (%2)?")
            .arg(QString::fromStdString(currency->name))
            .arg(QString::fromStdString(currency->iso_code)),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        BOOST_LOG_SEV(lg(), info) << "Delete cancelled by user";
        return;
    }

    // Store currency ISO code for the async operation
    const std::string iso_code = currency->iso_code;

    // Send delete request asynchronously
    QFuture<std::pair<bool, std::string>> future =
        QtConcurrent::run([this, iso_code]() -> std::pair<bool, std::string> {
            BOOST_LOG_SEV(lg(), info) << "Sending delete_currency_request for: "
                                      << iso_code;

            risk::messaging::delete_currency_request request{iso_code};
            auto payload = request.serialize();

            comms::protocol::frame request_frame(
                comms::protocol::message_type::delete_currency_request,
                0,
                std::move(payload)
            );

            // Send request synchronously (on background thread)
            auto response_result = client_->send_request_sync(std::move(request_frame));

            if (!response_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to send delete request";
                return {false, "Failed to communicate with server"};
            }

            BOOST_LOG_SEV(lg(), info) << "Received delete_currency_response";
            auto response = risk::messaging::delete_currency_response::deserialize(
                response_result->payload()
            );

            if (!response) {
                BOOST_LOG_SEV(lg(), error) << "Failed to deserialize response";
                return {false, "Invalid server response"};
            }

            return {response->success, response->message};
        });

    // Use a watcher to handle the result
    auto* watcher = new QFutureWatcher<std::pair<bool, std::string>>(this);
    connect(watcher, &QFutureWatcher<std::pair<bool, std::string>>::finished,
            this, [this, watcher, iso_code]() {
        auto [success, message] = watcher->result();
        watcher->deleteLater();

        if (success) {
            BOOST_LOG_SEV(lg(), info) << "Currency deleted successfully";

            // Emit status message
            emit statusChanged(QString("Successfully deleted currency: %1")
                .arg(QString::fromStdString(iso_code)));

            // Refresh the table to show updated data
            currencyModel_->refresh();
        } else {
            BOOST_LOG_SEV(lg(), error) << "Currency deletion failed: " << message;

            // Emit error message
            emit errorOccurred(QString("Failed to delete currency: %1")
                .arg(QString::fromStdString(message)));

            MessageBoxHelper::critical(this, "Delete Failed",
                QString::fromStdString(message));
        }
    });

    watcher->setFuture(future);
}

}


