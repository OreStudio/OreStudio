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
#include <vector>
#include <QtCore/QVariant>
#include <QtCore/QTimer>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <QtWidgets/QWidget>
#include <QtWidgets/QHeaderView>
#include <QtWidgets/QTableView>
#include <QtWidgets/QApplication>
#include <QFileDialog>
#include <QDesktopServices>
#include <QUrl>
#include <QMessageBox>
#include "ores.qt/CurrencyMdiWindow.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
// #include "ores.qt/CurrencyEditDialog.hpp" // Removed
#include "ores.qt/CurrencyItemDelegate.hpp" // Include the new delegate header
#include "ores.risk/messaging/protocol.hpp"
#include "ores.comms/protocol/frame.hpp"
#include "ores.risk/csv/exporter.hpp"
#include "ores.risk/orexml/exporter.hpp"

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
    currencyTableView_->setSelectionMode(QAbstractItemView::ExtendedSelection);
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
    const int selection_count = currencyTableView_->selectionModel()->selectedRows().count();
    emit selectionChanged(selection_count);
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

void CurrencyMdiWindow::viewHistorySelected() {
    const auto selected = currencyTableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "View history requested but no row selected";
        return;
    }

    const auto* currency = currencyModel_->getCurrency(selected.first().row());
    if (!currency) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to get currency for history view";
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Emitting showCurrencyHistory for currency: "
                             << currency->iso_code;
    emit showCurrencyHistory(QString::fromStdString(currency->iso_code));
}

void CurrencyMdiWindow::deleteSelected() {
    const auto selected = currencyTableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Delete requested but no row selected";
        return;
    }

    // Collect all selected currencies
    std::vector<std::string> iso_codes;
    for (const auto& index : selected) {
        const auto* currency = currencyModel_->getCurrency(index.row());
        if (currency) {
            iso_codes.push_back(currency->iso_code);
        }
    }

    if (iso_codes.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "No valid currencies to delete";
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Delete request for " << iso_codes.size() << " currencies";

    // Confirm deletion
    QString confirmMessage;
    if (iso_codes.size() == 1) {
        const auto* currency = currencyModel_->getCurrency(selected.first().row());
        confirmMessage = QString("Are you sure you want to delete currency '%1' (%2)?")
            .arg(QString::fromStdString(currency->name))
            .arg(QString::fromStdString(currency->iso_code));
    } else {
        confirmMessage = QString("Are you sure you want to delete %1 currencies?")
            .arg(iso_codes.size());
    }

    auto reply = MessageBoxHelper::question(this, "Delete Currency",
        confirmMessage, QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        BOOST_LOG_SEV(lg(), info) << "Delete cancelled by user";
        return;
    }

    // Send delete requests asynchronously
    using DeleteResult = std::vector<std::pair<std::string, std::pair<bool, std::string>>>;
    QFuture<DeleteResult> future = QtConcurrent::run([this, iso_codes]() -> DeleteResult {
        DeleteResult results;

        for (const auto& iso_code : iso_codes) {
            BOOST_LOG_SEV(lg(), info) << "Sending delete_currency_request for: " << iso_code;

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
                BOOST_LOG_SEV(lg(), error) << "Failed to send delete request for: " << iso_code;
                results.push_back({iso_code, {false, "Failed to communicate with server"}});
                continue;
            }

            BOOST_LOG_SEV(lg(), info) << "Received delete_currency_response for: " << iso_code;
            auto response = risk::messaging::delete_currency_response::deserialize(
                response_result->payload()
            );

            if (!response) {
                BOOST_LOG_SEV(lg(), error) << "Failed to deserialize response for: " << iso_code;
                results.push_back({iso_code, {false, "Invalid server response"}});
                continue;
            }

            results.push_back({iso_code, {response->success, response->message}});
        }

        return results;
    });

    // Use a watcher to handle the results
    auto* watcher = new QFutureWatcher<DeleteResult>(this);
    connect(watcher, &QFutureWatcher<DeleteResult>::finished,
            this, [this, watcher]() {
        auto results = watcher->result();
        watcher->deleteLater();

        int success_count = 0;
        int failure_count = 0;
        QString first_error;

        for (const auto& [iso_code, result] : results) {
            auto [success, message] = result;

            if (success) {
                BOOST_LOG_SEV(lg(), info) << "Currency deleted successfully: " << iso_code;
                success_count++;

                // Emit deletion signal for each successful deletion
                emit currencyDeleted(QString::fromStdString(iso_code));
            } else {
                BOOST_LOG_SEV(lg(), error) << "Currency deletion failed: " << iso_code
                                           << " - " << message;
                failure_count++;

                if (first_error.isEmpty()) {
                    first_error = QString::fromStdString(message);
                }
            }
        }

        // Refresh the table once after all deletions
        currencyModel_->refresh();

        // Show summary status message
        if (failure_count == 0) {
            QString msg = success_count == 1
                ? "Successfully deleted 1 currency"
                : QString("Successfully deleted %1 currencies").arg(success_count);
            emit statusChanged(msg);
        } else if (success_count == 0) {
            QString msg = QString("Failed to delete %1 %2: %3")
                .arg(failure_count)
                .arg(failure_count == 1 ? "currency" : "currencies")
                .arg(first_error);
            emit errorOccurred(msg);
            MessageBoxHelper::critical(this, "Delete Failed", msg);
        } else {
            QString msg = QString("Deleted %1 %2, failed to delete %3 %4")
                .arg(success_count)
                .arg(success_count == 1 ? "currency" : "currencies")
                .arg(failure_count)
                .arg(failure_count == 1 ? "currency" : "currencies");
            emit statusChanged(msg);
            MessageBoxHelper::warning(this, "Partial Success", msg);
        }
    });

    watcher->setFuture(future);
}

void CurrencyMdiWindow::exportToCSV() {
    if (currencyModel_->rowCount() == 0) {
        QMessageBox::information(this, "No Data", "There are no currencies to export.");
        return;
    }

    // Get a copy of the current currencies from the model
    auto currencies = currencyModel_->getCurrencies();

    QString fileName = QFileDialog::getSaveFileName(this,
        "Export to CSV", 
        "currencies.csv",
        "CSV Files (*.csv);;All Files (*)");

    if (fileName.isEmpty()) {
        return; // User cancelled
    }

    try {
        // Export using the existing CSV exporter
        std::string csvData = risk::csv::exporter::export_currency_config(currencies);

        // Write the data to the file
        QFile file(fileName);
        if (!file.open(QIODevice::WriteOnly | QIODevice::Text)) {
            BOOST_LOG_SEV(lg(), error) << "Failed to open file for writing: " << fileName.toStdString();
            MessageBoxHelper::critical(this, "File Error", 
                QString("Could not open file for writing: %1").arg(fileName));
            return;
        }

        file.write(csvData.c_str(), csvData.length());
        file.close();

        // Open the file in the default application
        QDesktopServices::openUrl(QUrl::fromLocalFile(fileName));

        emit statusChanged(QString("Successfully exported currencies to %1").arg(fileName));
        BOOST_LOG_SEV(lg(), info) << "Successfully exported currencies to CSV: " << fileName.toStdString();

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Error exporting to CSV: " << e.what();
        MessageBoxHelper::critical(this, "Export Error", 
            QString("Error during CSV export: %1").arg(e.what()));
    }
}

void CurrencyMdiWindow::exportToXML() {
    if (currencyModel_->rowCount() == 0) {
        QMessageBox::information(this, "No Data", "There are no currencies to export.");
        return;
    }

    // Get a copy of the current currencies from the model
    auto currencies = currencyModel_->getCurrencies();

    QString fileName = QFileDialog::getSaveFileName(this,
        "Export to ORE XML", 
        "currencies.xml",
        "XML Files (*.xml);;All Files (*)");

    if (fileName.isEmpty()) {
        return; // User cancelled
    }

    try {
        // Export using the existing ORE XML exporter
        std::string xmlData = risk::orexml::exporter::export_currency_config(currencies);

        // Write the data to the file
        QFile file(fileName);
        if (!file.open(QIODevice::WriteOnly | QIODevice::Text)) {
            BOOST_LOG_SEV(lg(), error) << "Failed to open file for writing: " << fileName.toStdString();
            MessageBoxHelper::critical(this, "File Error", 
                QString("Could not open file for writing: %1").arg(fileName));
            return;
        }

        file.write(xmlData.c_str(), xmlData.length());
        file.close();

        // Open the file in the default application
        QDesktopServices::openUrl(QUrl::fromLocalFile(fileName));

        emit statusChanged(QString("Successfully exported currencies to %1").arg(fileName));
        BOOST_LOG_SEV(lg(), info) << "Successfully exported currencies to XML: " << fileName.toStdString();

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Error exporting to XML: " << e.what();
        MessageBoxHelper::critical(this, "Export Error", 
            QString("Error during XML export: %1").arg(e.what()));
    }
}

}


