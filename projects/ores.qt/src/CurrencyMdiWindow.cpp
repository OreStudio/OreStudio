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
#include <filesystem>
#include <QtCore/QVariant>
#include <QtCore/QTimer>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <QtWidgets/QWidget>
#include <QtWidgets/QHBoxLayout>
#include <QtWidgets/QHeaderView>
#include <QtWidgets/QTableView>
#include <QtWidgets/QScrollBar>
#include <QtWidgets/QApplication>
#include <QtWidgets/QPushButton>
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
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/CurrencyItemDelegate.hpp"
#include "ores.qt/ImportCurrencyDialog.hpp"
#include "ores.risk/messaging/protocol.hpp"
#include "ores.comms/messaging/frame.hpp"
#include "ores.risk/csv/exporter.hpp"
#include "ores.risk/orexml/exporter.hpp"
#include "ores.risk/orexml/importer.hpp"

namespace ores::qt {

using comms::messaging::message_type;
using namespace ores::utility::log;

CurrencyMdiWindow::
CurrencyMdiWindow(ClientManager* clientManager,
                  ImageCache* imageCache,
                  const QString& username,
                  QWidget* parent)
    : QWidget(parent),
      verticalLayout_(new QVBoxLayout(this)),
      currencyTableView_(new QTableView(this)),
      toolBar_(new QToolBar(this)),
      pagination_widget_(new PaginationWidget(this)),
      reloadAction_(new QAction("Reload", this)),
      pulseTimer_(new QTimer(this)),
      addAction_(new QAction("Add", this)),
      editAction_(new QAction("Edit", this)),
      deleteAction_(new QAction("Delete", this)),
      currencyModel_(std::make_unique<ClientCurrencyModel>(clientManager, imageCache)),
      clientManager_(clientManager),
      imageCache_(imageCache),
      username_(username) {

    BOOST_LOG_SEV(lg(), debug) << "Creating currency MDI window";

    toolBar_->setMovable(false);
    toolBar_->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    const auto& iconColor = color_constants::icon_color;

    // Setup reload action with normal and stale icons
    setupReloadAction();
    toolBar_->addAction(reloadAction_);

    toolBar_->addSeparator();

    addAction_->setIcon(IconUtils::createRecoloredIcon(
            ":/icons/ic_fluent_add_20_regular.svg", iconColor));
    addAction_->setToolTip("Add new currency");
    connect(addAction_, &QAction::triggered, this, &CurrencyMdiWindow::addNew);
    toolBar_->addAction(addAction_);

    editAction_->setIcon(IconUtils::createRecoloredIcon(
            ":/icons/ic_fluent_edit_20_regular.svg", iconColor));
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

    auto importXMLAction = new QAction("Import XML", this);
    importXMLAction->setIcon(IconUtils::createRecoloredIcon(
            ":/icons/ic_fluent_document_code_16_regular.svg", iconColor));
    importXMLAction->setToolTip("Import currencies from ORE XML");
    connect(importXMLAction, &QAction::triggered, this,
        &CurrencyMdiWindow::importFromXML);
    toolBar_->addAction(importXMLAction);

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
    verticalLayout_->addWidget(pagination_widget_);

    currencyTableView_->setObjectName("currencyTableView");
    currencyTableView_->setAlternatingRowColors(true);
    currencyTableView_->setSelectionMode(QAbstractItemView::ExtendedSelection);
    currencyTableView_->setSelectionBehavior(QAbstractItemView::SelectRows);
    currencyTableView_->setWordWrap(false); // Prevent text wrapping in cells

    // Setup proxy model for sorting
    proxyModel_ = new QSortFilterProxyModel(this);
    proxyModel_->setSourceModel(currencyModel_.get());
    currencyTableView_->setModel(proxyModel_);
    currencyTableView_->setSortingEnabled(true);
    currencyTableView_->sortByColumn(0, Qt::AscendingOrder);  // Default sort by name

    currencyTableView_->setItemDelegate(
        new CurrencyItemDelegate(currencyTableView_));

    QHeaderView* horizontalHeader(currencyTableView_->horizontalHeader());
    currencyTableView_->verticalHeader()->setSectionResizeMode(QHeaderView::Fixed);
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

    // Connect pagination widget signals
    connect(pagination_widget_, &PaginationWidget::page_size_changed,
            this, [this](std::uint32_t size) {
        BOOST_LOG_SEV(lg(), debug) << "Page size changed to: " << size;
        currencyModel_->set_page_size(size);
        currencyModel_->refresh(true); // Reload from start with new page size
    });

    connect(pagination_widget_, &PaginationWidget::load_all_requested,
            this, [this]() {
        BOOST_LOG_SEV(lg(), debug) << "Load all requested from pagination widget";
        const auto total = currencyModel_->total_available_count();
        if (total > 0 && total <= 1000) {
            emit statusChanged("Loading all currencies...");
            currencyModel_->set_page_size(total);
            currencyModel_->refresh(true);
        } else if (total > 1000) {
            BOOST_LOG_SEV(lg(), warn) << "Total count " << total
                                      << " exceeds maximum page size of 1000";
            emit statusChanged("Cannot load all - too many records (max 1000)");
        }
    });

    // Connect connection state signals
    if (clientManager_) {
        connect(clientManager_, &ClientManager::connected, this, &CurrencyMdiWindow::onConnectionStateChanged);
        connect(clientManager_, &ClientManager::disconnected, this, &CurrencyMdiWindow::onConnectionStateChanged);
    }

    updateActionStates();

    emit statusChanged("Loading currencies...");

    // Initial load
    if (clientManager_->isConnected()) {
        currencyModel_->refresh();
    } else {
        emit statusChanged("Disconnected - Offline");
        toolBar_->setEnabled(false);
    }
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

void CurrencyMdiWindow::onConnectionStateChanged() {
    const bool connected = clientManager_ && clientManager_->isConnected();
    toolBar_->setEnabled(connected);
    
    if (connected) {
        emit statusChanged("Connected");
        // Optionally auto-refresh on reconnect?
        // currencyModel_->refresh(); 
    } else {
        emit statusChanged("Disconnected - Offline");
    }
}

void CurrencyMdiWindow::reload() {
    BOOST_LOG_SEV(lg(), debug) << "Reload requested";
    if (!clientManager_->isConnected()) {
        emit statusChanged("Cannot reload - Disconnected");
        return;
    }
    emit statusChanged("Reloading currencies...");
    clearStaleIndicator();
    currencyModel_->refresh();
}

void CurrencyMdiWindow::addNew() {
    BOOST_LOG_SEV(lg(), debug) << "Add new currency requested";
    emit addNewRequested();
}

void CurrencyMdiWindow::onDataLoaded() {
    const auto loaded = currencyModel_->rowCount();
    const auto total = currencyModel_->total_available_count();

    // Update pagination widget
    pagination_widget_->update_state(loaded, total);

    // Enable/disable Load All button based on whether there's more data
    const bool has_more = loaded < total && total > 0 && total <= 1000;
    BOOST_LOG_SEV(lg(), debug) << "onDataLoaded: loaded=" << loaded
                               << ", total=" << total
                               << ", has_more=" << has_more;
    pagination_widget_->set_load_all_enabled(has_more);

    const QString message = QString("Loaded %1 of %2 currencies")
                              .arg(loaded)
                              .arg(total);
    emit statusChanged(message);
    BOOST_LOG_SEV(lg(), debug) << "Currency data loaded successfully: "
                             << loaded << " of " << total << " currencies";

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

    // Map proxy index to source index
    const auto sourceIndex = proxyModel_->mapToSource(index);
    const auto* currency = currencyModel_->getCurrency(sourceIndex.row());
    if (!currency) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to get currency for row: "
                                 << sourceIndex.row();
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

    // Map proxy index to source index
    const auto sourceIndex = proxyModel_->mapToSource(selected.first());
    const auto* currency = currencyModel_->getCurrency(sourceIndex.row());
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

    if (!clientManager_->isConnected()) {
         MessageBoxHelper::warning(this, "Disconnected", "Cannot delete currency while disconnected.");
         return;
    }

    std::vector<std::string> iso_codes;
    for (const auto& index : selected) {
        // Map proxy index to source index
        const auto sourceIndex = proxyModel_->mapToSource(index);
        const auto* currency = currencyModel_->getCurrency(sourceIndex.row());
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
        const auto sourceIndex = proxyModel_->mapToSource(selected.first());
        const auto* currency = currencyModel_->getCurrency(sourceIndex.row());
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
    
    // Capture iso_codes by value
    auto task = [self, iso_codes]() -> DeleteResult {
        DeleteResult results;
        if (!self) return {};
        
        BOOST_LOG_SEV(lg(), debug) << "Making batch delete request for "
                                   << iso_codes.size() << " currencies";

        // Create batch request with all ISO codes
        risk::messaging::delete_currency_request request{iso_codes};
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            message_type::delete_currency_request,
            0, std::move(payload)
        );

        // Send single batch request via ClientManager
        auto response_result = self->clientManager_->sendRequest(
            std::move(request_frame));

        if (!response_result) {
            BOOST_LOG_SEV(lg(), error) << "Failed to send batch delete request";
            // If network fails, mark all as failed
            for (const auto& iso_code : iso_codes) {
                results.push_back({iso_code,
                    {false, "Failed to communicate with server"}});
            }
            return results;
        }

        BOOST_LOG_SEV(lg(), debug) << "Received batch delete_currency_response";

        // Decompress payload
        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) {
            BOOST_LOG_SEV(lg(), error) << "Failed to decompress batch response";
            for (const auto& iso_code : iso_codes) {
                results.push_back({iso_code,
                    {false, "Failed to decompress server response"}});
            }
            return results;
        }

        // Deserialize batch response
        auto response = risk::messaging::delete_currency_response::
            deserialize(*payload_result);

        if (!response) {
            BOOST_LOG_SEV(lg(), error) << "Failed to deserialize batch response";
            // If deserialize fails, mark all as failed
            for (const auto& iso_code : iso_codes) {
                results.push_back({iso_code,
                    {false, "Invalid server response"}});
            }
            return results;
        }

        // Convert batch results to expected format
        for (const auto& result : response->results) {
            results.push_back({result.iso_code,
                {result.success, result.message}});
        }

        return results;
    };

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

    // Run async
    QFuture<DeleteResult> future = QtConcurrent::run(task);
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

void CurrencyMdiWindow::importFromXML() {
    BOOST_LOG_SEV(lg(), debug) << "Import XML action triggered";

    if (!clientManager_->isConnected()) {
         MessageBoxHelper::warning(this, "Disconnected", "Cannot import currencies while disconnected.");
         return;
    }

    QString fileName = QFileDialog::getOpenFileName(
        this,
        "Select ORE XML File to Import",
        QString(),
        "ORE XML Files (*.xml);;All Files (*)"
    );

    if (fileName.isEmpty()) {
        BOOST_LOG_SEV(lg(), debug) << "User cancelled file selection";
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Selected file for import: "
                              << fileName.toStdString();
    emit statusChanged("Parsing XML file...");

    try {
        // Parse the XML file using the existing importer
        using risk::orexml::importer;
        std::filesystem::path path(fileName.toStdString());
        auto currencies = importer::import_currency_config(path);

        if (currencies.empty()) {
            BOOST_LOG_SEV(lg(), warn) << "No currencies found in XML file";
            MessageBoxHelper::information(this, "No Currencies Found",
                "The selected XML file does not contain any currencies.");
            emit statusChanged("Import cancelled - no currencies found");
            return;
        }

        BOOST_LOG_SEV(lg(), info) << "Parsed " << currencies.size()
                                  << " currencies from XML";

        emit statusChanged(QString("Found %1 currencies - opening import dialog...")
            .arg(currencies.size()));

        // Show import dialog with full preview and import capability
        // Assuming ImportCurrencyDialog is updated to take ClientManager
        auto* dialog = new ImportCurrencyDialog(currencies, fileName, clientManager_,
                                                 username_, this);

        // Connect completion signal to refresh UI
        connect(dialog, &ImportCurrencyDialog::importCompleted,
                this, [this](int success_count, int total_count) {
            BOOST_LOG_SEV(lg(), info)
                << "Import complete: " << success_count
                << " of " << total_count << " currencies imported";

            if (success_count > 0) {
                // Refresh the currency list to show imported currencies
                currencyModel_->refresh();

                QString message = QString(
                    "Successfully imported %1 of %2 currencies")
                    .arg(success_count).arg(total_count);
                emit statusChanged(message);
                MessageBoxHelper::information(this, "Import Complete", message);
            } else {
                emit statusChanged("Import failed - no currencies imported");
                MessageBoxHelper::warning(this, "Import Failed",
                    "Failed to import currencies. Check the log for details.");
            }
        });

        // Connect cancellation signal
        connect(dialog, &ImportCurrencyDialog::importCancelled,
                this, [this]() {
            BOOST_LOG_SEV(lg(), debug) << "Import cancelled by user";
            emit statusChanged("Import cancelled");
        });

        // Show dialog modally
        if (dialog->exec() != QDialog::Accepted) {
            BOOST_LOG_SEV(lg(), debug) << "User cancelled import dialog";
            emit statusChanged("Import cancelled");
        }

        dialog->deleteLater();

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Error importing XML: " << e.what();
        MessageBoxHelper::critical(this, "Import Error",
            QString("Failed to import XML file:\n%1").arg(e.what()));
        emit statusChanged("Import failed");
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

void CurrencyMdiWindow::setupReloadAction() {
    normalReloadIcon_ = IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_arrow_clockwise_16_regular.svg", color_constants::icon_color);
    staleReloadIcon_ = IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_arrow_clockwise_16_regular.svg", color_constants::stale_indicator);

    reloadAction_->setIcon(normalReloadIcon_);
    reloadAction_->setToolTip("Reload currencies from server");
    connect(reloadAction_, &QAction::triggered, this, &CurrencyMdiWindow::reload);

    // Setup pulse animation timer
    connect(pulseTimer_, &QTimer::timeout, this, [this]() {
        pulseState_ = !pulseState_;
        reloadAction_->setIcon(pulseState_ ? staleReloadIcon_ : normalReloadIcon_);

        pulseCount_++;
        // Stop pulsing after 6 cycles (3 seconds) but keep stale icon
        if (pulseCount_ >= 6) {
            pulseTimer_->stop();
            reloadAction_->setIcon(staleReloadIcon_);
        }
    });
}

void CurrencyMdiWindow::startPulseAnimation() {
    pulseCount_ = 0;
    pulseState_ = false;
    pulseTimer_->start(500);  // Toggle every 500ms
}

void CurrencyMdiWindow::stopPulseAnimation() {
    pulseTimer_->stop();
    reloadAction_->setIcon(normalReloadIcon_);
}

void CurrencyMdiWindow::markAsStale() {
    if (!isStale_) {
        isStale_ = true;
        reloadAction_->setToolTip("Data changed on server - click to reload");
        startPulseAnimation();
        BOOST_LOG_SEV(lg(), info) << "Currency data marked as stale";
    }
}

void CurrencyMdiWindow::clearStaleIndicator() {
    if (isStale_) {
        isStale_ = false;
        stopPulseAnimation();
        reloadAction_->setToolTip("Reload currencies from server");
        BOOST_LOG_SEV(lg(), debug) << "Stale indicator cleared";
    }
}

}
