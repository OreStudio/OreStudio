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
#include "ores.qt/CurrencyMdiWindow.hpp"

#include <vector>
#include <QtCore/QVariant>
#include <QtCore/QTimer>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <QtWidgets/QWidget>
#include <QtWidgets/QHeaderView>
#include <QtWidgets/QTableView>
#include <QtWidgets/QScrollBar>
#include <QtWidgets/QApplication>
#include <QFileDialog>
#include <QDesktopServices>
#include <QUrl>
#include <QMessageBox>
#include <QToolBar>
#include <QAction>
#include <QPixmap>
#include <QImage>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/CurrencyItemDelegate.hpp"
#include "ores.risk/messaging/protocol.hpp"
#include "ores.comms/protocol/frame.hpp"
#include "ores.risk/csv/exporter.hpp"
#include "ores.risk/orexml/exporter.hpp"

namespace ores::qt {

using comms::protocol::message_type;
using namespace ores::utility::log;

CurrencyMdiWindow::
CurrencyMdiWindow(std::shared_ptr<comms::net::client> client, QWidget* parent)
    : QWidget(parent),
      verticalLayout_(new QVBoxLayout(this)),
      currencyTableView_(new QTableView(this)),
      toolBar_(new QToolBar(this)),
      addAction_(new QAction("Add", this)),
      editAction_(new QAction("Edit", this)),
      deleteAction_(new QAction("Delete", this)),
      currencyModel_(new ClientCurrencyModel(client)),
      client_(std::move(client)) {

    BOOST_LOG_SEV(lg(), debug) << "Creating currency MDI window";

    toolBar_->setMovable(false);
    toolBar_->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    const QColor iconColor(220, 220, 220);

    auto reloadAction = new QAction("Reload", this);
    reloadAction->setIcon(IconUtils::createRecoloredIcon(
            ":/icons/ic_fluent_arrow_clockwise_16_regular.svg", iconColor));
    reloadAction->setToolTip("Reload currencies from server");
    connect(reloadAction, &QAction::triggered, this,
        &CurrencyMdiWindow::reload);
    toolBar_->addAction(reloadAction);

    toolBar_->addSeparator();

    addAction_->setIcon(IconUtils::createRecoloredIcon(
            ":/icons/ic_fluent_add_20_filled.svg", iconColor));
    addAction_->setToolTip("Add new currency");
    connect(addAction_, &QAction::triggered, this, &CurrencyMdiWindow::addNew);
    toolBar_->addAction(addAction_);

    editAction_->setIcon(IconUtils::createRecoloredIcon(
            ":/icons/ic_fluent_edit_20_filled.svg", iconColor));
    editAction_->setToolTip("Edit selected currency");
    connect(editAction_, &QAction::triggered, this,
        &CurrencyMdiWindow::editSelected);
    toolBar_->addAction(editAction_);


    deleteAction_->setIcon(IconUtils::createRecoloredIcon(
            ":/icons/ic_fluent_delete_20_regular.svg", iconColor));
    deleteAction_->setToolTip("Delete selected currency/currencies");
    connect(deleteAction_, &QAction::triggered, this,
        &CurrencyMdiWindow::deleteSelected);
    toolBar_->addAction(deleteAction_);

    historyAction_ = new QAction("History", this);
    historyAction_->setIcon(IconUtils::createRecoloredIcon(
            ":/icons/ic_fluent_history_20_regular.svg", iconColor));
    historyAction_->setToolTip("View currency history");
    connect(historyAction_, &QAction::triggered, this,
        &CurrencyMdiWindow::viewHistorySelected);
    toolBar_->addAction(historyAction_);

    toolBar_->addSeparator();

    auto exportCSVAction = new QAction("Export CSV", this);
    exportCSVAction->setIcon(IconUtils::createRecoloredIcon(
            ":/icons/ic_fluent_document_table_20_regular.svg", iconColor));
    exportCSVAction->setToolTip("Export currencies to CSV");
    connect(exportCSVAction, &QAction::triggered, this,
        &CurrencyMdiWindow::exportToCSV);
    toolBar_->addAction(exportCSVAction);

    auto exportXMLAction = new QAction("Export XML", this);
    exportXMLAction->setIcon(IconUtils::createRecoloredIcon(
            ":/icons/ic_fluent_document_code_16_regular.svg", iconColor));
    exportXMLAction->setToolTip("Export currencies to ORE XML");
    connect(exportXMLAction, &QAction::triggered, this,
        &CurrencyMdiWindow::exportToXML);
    toolBar_->addAction(exportXMLAction);

    verticalLayout_->addWidget(toolBar_);


    verticalLayout_->addWidget(currencyTableView_);

    currencyTableView_->setObjectName("currencyTableView");
    currencyTableView_->setAlternatingRowColors(true);
    currencyTableView_->setSelectionMode(QAbstractItemView::ExtendedSelection);
    currencyTableView_->setSelectionBehavior(QAbstractItemView::SelectRows);
    currencyTableView_->resizeRowsToContents();

    currencyTableView_->setModel(currencyModel_.get());

    currencyTableView_->setItemDelegate(
        new CurrencyItemDelegate(currencyTableView_));

    QHeaderView* verticalHeader(currencyTableView_->verticalHeader());
    QHeaderView* horizontalHeader(currencyTableView_->horizontalHeader());
    verticalHeader->setSectionResizeMode(QHeaderView::ResizeToContents);
    horizontalHeader->setSectionResizeMode(QHeaderView::ResizeToContents);

    // Connect signals
    connect(currencyModel_.get(), &ClientCurrencyModel::dataLoaded,
            this, &CurrencyMdiWindow::onDataLoaded);
    connect(currencyModel_.get(), &ClientCurrencyModel::loadError,
            this, &CurrencyMdiWindow::onLoadError);
    connect(currencyTableView_, &QTableView::doubleClicked,
            this, &CurrencyMdiWindow::onRowDoubleClicked);
    connect(currencyTableView_->selectionModel(),
        &QItemSelectionModel::selectionChanged,
            this, &CurrencyMdiWindow::onSelectionChanged);

    updateActionStates();

    emit statusChanged("Loading currencies...");

    currencyModel_->refresh();
}

CurrencyMdiWindow::~CurrencyMdiWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Destroying currency MDI window";

    // Disconnect and cancel any active QFutureWatcher objects
    const auto watchers = findChildren<QFutureWatcherBase*>();
    for (auto* watcher : watchers) {
        disconnect(watcher, nullptr, this, nullptr);
        watcher->cancel();
        watcher->waitForFinished();
    }
}

void CurrencyMdiWindow::reload() {
    BOOST_LOG_SEV(lg(), debug) << "Reload requested";
    emit statusChanged("Reloading currencies...");
    currencyModel_->refresh();
}

void CurrencyMdiWindow::addNew() {
    BOOST_LOG_SEV(lg(), debug) << "Add new currency requested";
    emit addNewRequested();
}

void CurrencyMdiWindow::onDataLoaded() {
    const QString message = QString("Loaded %1 currencies")
                              .arg(currencyModel_->rowCount());
    emit statusChanged(message);
    BOOST_LOG_SEV(lg(), debug) << "Currency data loaded successfully: "
                             << currencyModel_->rowCount() << " currencies";

    // Auto-select first row if data is available and nothing is selected
    if (currencyModel_->rowCount() > 0 &&
        currencyTableView_->selectionModel()->selectedRows().isEmpty()) {
        currencyTableView_->selectRow(0);
        BOOST_LOG_SEV(lg(), debug) << "Auto-selected first row";
    }
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

    BOOST_LOG_SEV(lg(), debug) << "Emitting showCurrencyDetails for currency: "
                             << currency->iso_code;
    emit showCurrencyDetails(*currency);
}

void CurrencyMdiWindow::onSelectionChanged() {
    const int selection_count = currencyTableView_->selectionModel()->selectedRows().count();
    updateActionStates();
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

    BOOST_LOG_SEV(lg(), debug) << "Emitting showCurrencyHistory for currency: "
                             << currency->iso_code;
    emit showCurrencyHistory(QString::fromStdString(currency->iso_code));
}

void CurrencyMdiWindow::deleteSelected() {
    const auto selected = currencyTableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Delete requested but no row selected";
        return;
    }

    std::vector<std::string> iso_codes;
    for (const auto& index : selected) {
        const auto* currency = currencyModel_
            ->getCurrency(index.row());
        if (currency)
            iso_codes.push_back(currency->iso_code);
    }

    if (iso_codes.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "No valid currencies to delete";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Delete requested for " << iso_codes.size()
                               << " currencies";

    QString confirmMessage;
    if (iso_codes.size() == 1) {
        const auto* currency =
            currencyModel_->getCurrency(selected.first().row());
        confirmMessage = QString("Are you sure you want to delete currency '%1' (%2)?")
            .arg(QString::fromStdString(currency->name))
            .arg(QString::fromStdString(currency->iso_code));
    } else {
        confirmMessage = QString("Are you sure you want to delete %1 currencies?")
            .arg(iso_codes.size());
    }

    auto reply =
        MessageBoxHelper::question(this, "Delete Currency",
        confirmMessage, QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        BOOST_LOG_SEV(lg(), debug) << "Delete cancelled by user.";
        return;
    }

    QPointer<CurrencyMdiWindow> self = this;
    using DeleteResult = std::vector<std::pair<std::string, std::pair<bool, std::string>>>;
    QFuture<DeleteResult> future =
        QtConcurrent::run([self, iso_codes]() -> DeleteResult {
        DeleteResult results;

        BOOST_LOG_SEV(lg(), debug) << "Making a delete currencies request.";
        if (!self) return {};

        for (const auto& iso_code : iso_codes) {
            BOOST_LOG_SEV(lg(), debug) << "Sending delete_currency_request for: "
                                       << iso_code;

            risk::messaging::delete_currency_request request{iso_code};
            auto payload = request.serialize();

            comms::protocol::frame request_frame(
                message_type::delete_currency_request,
                0, std::move(payload)
            );

            auto response_result = self->client_->
                send_request_sync(std::move(request_frame));

            if (!response_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to send delete request for: "
                                           << iso_code;
                results.push_back({
                        iso_code,
                        { false, "Failed to communicate with server"}});
                continue;
            }

            BOOST_LOG_SEV(lg(), debug) << "Received delete_currency_response for: "
                                       << iso_code;
            auto response = risk::messaging::delete_currency_response::
                deserialize(response_result->payload());

            if (!response) {
                BOOST_LOG_SEV(lg(), error) << "Failed to deserialize response for: "
                                           << iso_code;
                results.push_back({iso_code, {false, "Invalid server response"}});
                continue;
            }

            results.push_back({iso_code, {response->success, response->message}});
        }

        return results;
    });

    auto* watcher = new QFutureWatcher<DeleteResult>(self);
    connect(watcher, &QFutureWatcher<DeleteResult>::finished,
            self, [self, watcher]() {
        auto results = watcher->result();
        watcher->deleteLater();

        int success_count = 0;
        int failure_count = 0;
        QString first_error;

        for (const auto& [iso_code, result] : results) {
            auto [success, message] = result;

            if (success) {
                BOOST_LOG_SEV(lg(), debug) << "Currency deleted successfully: " << iso_code;
                success_count++;

                // Emit deletion signal for each successful deletion
                emit self->currencyDeleted(QString::fromStdString(iso_code));
            } else {
                BOOST_LOG_SEV(lg(), error) << "Currency deletion failed: " << iso_code
                                           << " - " << message;
                failure_count++;

                if (first_error.isEmpty()) {
                    first_error = QString::fromStdString(message);
                }
            }
        }

        self->currencyModel_->refresh();
        if (failure_count == 0) {
            QString msg = success_count == 1
                ? "Successfully deleted 1 currency"
                : QString("Successfully deleted %1 currencies").arg(success_count);
            emit self->statusChanged(msg);
        } else if (success_count == 0) {
            QString msg = QString("Failed to delete %1 %2: %3")
                .arg(failure_count)
                .arg(failure_count == 1 ? "currency" : "currencies")
                .arg(first_error);
            emit self->errorOccurred(msg);
            MessageBoxHelper::critical(self, "Delete Failed", msg);
        } else {
            QString msg = QString("Deleted %1 %2, failed to delete %3 %4")
                .arg(success_count)
                .arg(success_count == 1 ? "currency" : "currencies")
                .arg(failure_count)
                .arg(failure_count == 1 ? "currency" : "currencies");
            emit self->statusChanged(msg);
            MessageBoxHelper::warning(self, "Partial Success", msg);
        }
    });

    watcher->setFuture(future);
}

void CurrencyMdiWindow::exportToCSV() {
    if (currencyModel_->rowCount() == 0) {
        BOOST_LOG_SEV(lg(), debug) << "User requested CSV export but "
                                   << "there are no currencies to export.";
        QMessageBox::information(this, "No Data",
            "There are no currencies to export.");
        return;
    }

    // Get a copy of the current currencies from the model
    auto currencies = currencyModel_->getCurrencies();

    QString fileName = QFileDialog::getSaveFileName(this,
        "Export to CSV",
        "currencies.csv",
        "CSV Files (*.csv);;All Files (*)");

    if (fileName.isEmpty()) {
        BOOST_LOG_SEV(lg(), debug) << "User cancelled file selection in export.";
        return;
    }

    try {
        // Export using the existing CSV exporter
        using risk::csv::exporter;
        std::string csvData = exporter::export_currency_config(currencies);

        QFile file(fileName);
        if (!file.open(QIODevice::WriteOnly | QIODevice::Text)) {
            BOOST_LOG_SEV(lg(), error) << "Failed to open file for writing: "
                                       << fileName.toStdString();
            MessageBoxHelper::critical(this, "File Error",
                QString("Could not open file for writing: %1").arg(fileName));
            return;
        }

        file.write(csvData.c_str(), csvData.length());
        file.close();

        // Open the file in the default application
        QDesktopServices::openUrl(QUrl::fromLocalFile(fileName));

        emit statusChanged(QString("Successfully exported currencies to %1")
            .arg(fileName));
        BOOST_LOG_SEV(lg(), debug) << "Successfully exported currencies to CSV: "
                                   << fileName.toStdString();

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Error exporting to CSV: " << e.what();
        MessageBoxHelper::critical(this, "Export Error",
            QString("Error during CSV export: %1").arg(e.what()));
    }
}

void CurrencyMdiWindow::exportToXML() {
    if (currencyModel_->rowCount() == 0) {
        BOOST_LOG_SEV(lg(), debug) << "User requested CSV export but "
                                   << "there are no currencies to export.";
        QMessageBox::information(this, "No Data",
            "There are no currencies to export.");
        return;
    }

    const auto currencies = currencyModel_->getCurrencies();

    QString fileName = QFileDialog::getSaveFileName(this,
        "Export to ORE XML",
        "currencies.xml",
        "XML Files (*.xml);;All Files (*)");

    if (fileName.isEmpty()) {
        BOOST_LOG_SEV(lg(), debug) << "User cancelled file selection in export.";
        return;
    }

    try {
        using risk::orexml::exporter;
        std::string xmlData = exporter::export_currency_config(currencies);

        QFile file(fileName);
        if (!file.open(QIODevice::WriteOnly | QIODevice::Text)) {
            BOOST_LOG_SEV(lg(), error) << "Failed to open file for writing: "
                                       << fileName.toStdString();
            MessageBoxHelper::critical(this, "File Error",
                QString("Could not open file for writing: %1")
                .arg(fileName));
            return;
        }

        file.write(xmlData.c_str(), xmlData.length());
        file.close();

        QDesktopServices::openUrl(QUrl::fromLocalFile(fileName));

        emit statusChanged(QString("Successfully exported currencies to %1")
            .arg(fileName));
        BOOST_LOG_SEV(lg(), debug) << "Successfully exported currencies to XML: "
                                   << fileName.toStdString();

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Error exporting to XML: " << e.what();
        MessageBoxHelper::critical(this, "Export Error",
            QString("Error during XML export: %1").arg(e.what()));
    }
}

QSize CurrencyMdiWindow::sizeHint() const {
    // Define a sensible, consistent default size for this MDI window. This is
    // the preferred minimum size for a good user experience.
    const int minimumWidth = 1000;
    const int minimumHeight = 600;

    // Get the base size hint calculated by the QVBoxLayout and its contents.
    // This ensures that if the toolbar or table headers require a width > 1000,
    // the size hint respects that.
    QSize baseSize = QWidget::sizeHint();

    // Return the maximum of the base size and our defined minimum size. This
    // guarantees the window will open at a reasonable size but can grow if
    // required by the layout manager.
    return { qMax(baseSize.width(), minimumWidth),
             qMax(baseSize.height(), minimumHeight) };
}

void CurrencyMdiWindow::updateActionStates() {
    const int selection_count = currencyTableView_
        ->selectionModel()->selectedRows().count();
    const bool hasSelection = selection_count > 0;

    // Enable/disable actions based on selection
    editAction_->setEnabled(hasSelection);
    deleteAction_->setEnabled(hasSelection);
    historyAction_->setEnabled(hasSelection);
}

}
