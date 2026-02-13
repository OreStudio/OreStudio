/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.qt/CountryMdiWindow.hpp"

#include <vector>
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
#include <QFileDialog>
#include <QDesktopServices>
#include <QUrl>
#include <QMessageBox>
#include <QToolBar>
#include <QAction>
#include <QMenu>
#include <QSettings>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/EntityItemDelegate.hpp"
#include "ores.refdata/messaging/protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using comms::messaging::message_type;
using namespace ores::logging;

CountryMdiWindow::
CountryMdiWindow(ClientManager* clientManager,
                 ImageCache* imageCache,
                 const QString& username,
                 QWidget* parent)
    : EntityListMdiWindow(parent),
      verticalLayout_(new QVBoxLayout(this)),
      countryTableView_(new QTableView(this)),
      toolBar_(new QToolBar(this)),
      pagination_widget_(new PaginationWidget(this)),
      reloadAction_(new QAction("Reload", this)),
      addAction_(new QAction("Add", this)),
      editAction_(new QAction("Edit", this)),
      deleteAction_(new QAction("Delete", this)),
      countryModel_(std::make_unique<ClientCountryModel>(clientManager, imageCache)),
      clientManager_(clientManager),
      imageCache_(imageCache),
      username_(username) {

    BOOST_LOG_SEV(lg(), debug) << "Creating country MDI window";

    toolBar_->setMovable(false);
    toolBar_->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);

    // Setup reload action with normal and stale icons
    setupReloadAction();
    toolBar_->addAction(reloadAction_);

    toolBar_->addSeparator();

    addAction_->setIcon(IconUtils::createRecoloredIcon(
            Icon::Add, IconUtils::DefaultIconColor));
    addAction_->setToolTip("Add new country");
    connect(addAction_, &QAction::triggered, this, &CountryMdiWindow::addNew);
    toolBar_->addAction(addAction_);

    editAction_->setIcon(IconUtils::createRecoloredIcon(
            Icon::Edit, IconUtils::DefaultIconColor));
    editAction_->setToolTip("Edit selected country");
    connect(editAction_, &QAction::triggered, this,
        &CountryMdiWindow::editSelected);
    toolBar_->addAction(editAction_);

    deleteAction_->setIcon(IconUtils::createRecoloredIcon(
            Icon::Delete, IconUtils::DefaultIconColor));
    deleteAction_->setToolTip("Delete selected country/countries");
    connect(deleteAction_, &QAction::triggered, this,
        &CountryMdiWindow::deleteSelected);
    toolBar_->addAction(deleteAction_);

    historyAction_ = new QAction("History", this);
    historyAction_->setIcon(IconUtils::createRecoloredIcon(
            Icon::History, IconUtils::DefaultIconColor));
    historyAction_->setToolTip("View country history");
    connect(historyAction_, &QAction::triggered, this,
        &CountryMdiWindow::viewHistorySelected);
    toolBar_->addAction(historyAction_);

    toolBar_->addSeparator();

    auto exportCSVAction = new QAction("Export", this);
    exportCSVAction->setIcon(IconUtils::createRecoloredIcon(
            Icon::ExportCsv, IconUtils::DefaultIconColor));
    exportCSVAction->setToolTip("Export countries to CSV");
    connect(exportCSVAction, &QAction::triggered, this,
        &CountryMdiWindow::exportToCSV);
    toolBar_->addAction(exportCSVAction);

    verticalLayout_->addWidget(toolBar_);
    verticalLayout_->addWidget(countryTableView_);
    verticalLayout_->addWidget(pagination_widget_);

    countryTableView_->setObjectName("countryTableView");
    countryTableView_->setAlternatingRowColors(true);
    countryTableView_->setSelectionMode(QAbstractItemView::ExtendedSelection);
    countryTableView_->setSelectionBehavior(QAbstractItemView::SelectRows);
    countryTableView_->setWordWrap(false);

    // Setup proxy model for sorting
    proxyModel_ = new QSortFilterProxyModel(this);
    proxyModel_->setSourceModel(countryModel_.get());
    countryTableView_->setModel(proxyModel_);
    countryTableView_->setSortingEnabled(true);
    countryTableView_->sortByColumn(ClientCountryModel::Name, Qt::AscendingOrder);

    using cs = column_style;
    countryTableView_->setItemDelegate(new EntityItemDelegate({
        cs::icon_centered,    // Flag
        cs::text_left,        // Name
        cs::mono_bold_center, // Alpha2Code
        cs::mono_bold_center, // Alpha3Code
        cs::mono_center,      // NumericCode
        cs::text_left,        // OfficialName
        cs::mono_center,      // Version
        cs::text_left,        // ModifiedBy
        cs::mono_left         // RecordedAt
    }, countryTableView_));

    QHeaderView* horizontalHeader(countryTableView_->horizontalHeader());
    countryTableView_->verticalHeader()->setSectionResizeMode(QHeaderView::Fixed);
    horizontalHeader->setSectionResizeMode(QHeaderView::ResizeToContents);

    // Setup column visibility (context menu and defaults)
    setupColumnVisibility();

    // Restore saved settings (column visibility, window size)
    restoreSettings();

    // Connect signals
    connect(countryModel_.get(), &ClientCountryModel::dataLoaded,
            this, &CountryMdiWindow::onDataLoaded);
    connect(countryModel_.get(), &ClientCountryModel::loadError,
            this, &CountryMdiWindow::onLoadError);
    connect(countryTableView_, &QTableView::doubleClicked,
            this, &CountryMdiWindow::onRowDoubleClicked);
    connect(countryTableView_->selectionModel(),
        &QItemSelectionModel::selectionChanged,
            this, &CountryMdiWindow::onSelectionChanged);

    // Connect pagination widget signals
    connect(pagination_widget_, &PaginationWidget::page_size_changed,
            this, [this](std::uint32_t size) {
        BOOST_LOG_SEV(lg(), debug) << "Page size changed to: " << size;
        countryModel_->set_page_size(size);
        countryModel_->refresh(true);
    });

    connect(pagination_widget_, &PaginationWidget::load_all_requested,
            this, [this]() {
        BOOST_LOG_SEV(lg(), debug) << "Load all requested from pagination widget";
        const auto total = countryModel_->total_available_count();
        if (total > 0 && total <= 1000) {
            emit statusChanged("Loading all countries...");
            countryModel_->set_page_size(total);
            countryModel_->refresh(true);
        } else if (total > 1000) {
            BOOST_LOG_SEV(lg(), warn) << "Total count " << total
                                      << " exceeds maximum page size of 1000";
            emit statusChanged("Cannot load all - too many records (max 1000)");
        }
    });

    connect(pagination_widget_, &PaginationWidget::page_requested,
            this, [this](std::uint32_t offset, std::uint32_t limit) {
        BOOST_LOG_SEV(lg(), debug) << "Page requested: offset=" << offset
                                   << ", limit=" << limit;
        emit statusChanged("Loading countries...");
        countryModel_->load_page(offset, limit);
    });

    // Connect connection state signals
    if (clientManager_) {
        connect(clientManager_, &ClientManager::connected, this, &CountryMdiWindow::onConnectionStateChanged);
        connect(clientManager_, &ClientManager::disconnected, this, &CountryMdiWindow::onConnectionStateChanged);
    }

    updateActionStates();

    emit statusChanged("Loading countries...");

    // Initial load (only if logged in, not just connected)
    if (clientManager_->isLoggedIn()) {
        countryModel_->refresh();
    } else {
        emit statusChanged("Disconnected - Offline");
        toolBar_->setEnabled(false);
    }
}

CountryMdiWindow::~CountryMdiWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Destroying country MDI window";

    // Disconnect and cancel any active QFutureWatcher objects
    const auto watchers = findChildren<QFutureWatcherBase*>();
    for (auto* watcher : watchers) {
        disconnect(watcher, nullptr, this, nullptr);
        watcher->cancel();
        watcher->waitForFinished();
    }
}

void CountryMdiWindow::onConnectionStateChanged() {
    const bool connected = clientManager_ && clientManager_->isConnected();
    toolBar_->setEnabled(connected);

    if (connected) {
        emit statusChanged("Connected");
    } else {
        emit statusChanged("Disconnected - Offline");
    }
}

void CountryMdiWindow::reload() {
    BOOST_LOG_SEV(lg(), debug) << "Reload requested";
    if (!clientManager_->isConnected()) {
        emit statusChanged("Cannot reload - Disconnected");
        return;
    }
    emit statusChanged("Reloading countries...");
    clearStaleIndicator();

    countryModel_->refresh();
}

void CountryMdiWindow::addNew() {
    BOOST_LOG_SEV(lg(), debug) << "Add new country requested";
    emit addNewRequested();
}

void CountryMdiWindow::onDataLoaded() {
    const auto loaded = countryModel_->rowCount();
    const auto total = countryModel_->total_available_count();

    // Update pagination widget
    pagination_widget_->update_state(loaded, total);

    // Enable/disable Load All button based on whether there's more data
    const bool has_more = loaded < total && total > 0 && total <= 1000;
    BOOST_LOG_SEV(lg(), debug) << "onDataLoaded: loaded=" << loaded
                               << ", total=" << total
                               << ", has_more=" << has_more;
    pagination_widget_->set_load_all_enabled(has_more);

    const QString message = QString("Loaded %1 of %2 countries")
                              .arg(loaded)
                              .arg(total);
    emit statusChanged(message);
    BOOST_LOG_SEV(lg(), debug) << "Country data loaded successfully: "
                             << loaded << " of " << total << " countries";

    // Auto-select first row if data is available and nothing is selected
    if (countryModel_->rowCount() > 0 &&
        countryTableView_->selectionModel()->selectedRows().isEmpty()) {
        countryTableView_->selectRow(0);
        BOOST_LOG_SEV(lg(), debug) << "Auto-selected first row";
    }
}

void CountryMdiWindow::onLoadError(const QString& error_message,
                                    const QString& details) {
    emit errorOccurred(error_message);
    BOOST_LOG_SEV(lg(), error) << "Error loading countries: "
                              << error_message.toStdString();

    MessageBoxHelper::critical(this, tr("Load Error"), error_message, details);
}

void CountryMdiWindow::onRowDoubleClicked(const QModelIndex& index) {
    if (!index.isValid()) {
        return;
    }

    // Map proxy index to source index
    const auto sourceIndex = proxyModel_->mapToSource(index);
    const auto* country = countryModel_->getCountry(sourceIndex.row());
    if (!country) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to get country for row: "
                                 << sourceIndex.row();
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Emitting showCountryDetails for country: "
                             << country->alpha2_code;
    emit showCountryDetails(*country);
}

void CountryMdiWindow::onSelectionChanged() {
    const int selection_count = countryTableView_->selectionModel()->selectedRows().count();
    updateActionStates();
    emit selectionChanged(selection_count);
}

void CountryMdiWindow::editSelected() {
    const auto selected = countryTableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Edit requested but no row selected";
        return;
    }

    onRowDoubleClicked(selected.first());
}

void CountryMdiWindow::viewHistorySelected() {
    const auto selected = countryTableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "View history requested but no row selected";
        return;
    }

    const auto sourceIndex = proxyModel_->mapToSource(selected.first());
    const auto* country = countryModel_->getCountry(sourceIndex.row());
    if (!country) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to get country for history view";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Emitting showCountryHistory for country: "
                             << country->alpha2_code;
    emit showCountryHistory(QString::fromStdString(country->alpha2_code));
}

void CountryMdiWindow::deleteSelected() {
    const auto selected = countryTableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Delete requested but no row selected";
        return;
    }

    if (!clientManager_->isConnected()) {
         MessageBoxHelper::warning(this, "Disconnected", "Cannot delete country while disconnected.");
         return;
    }

    std::vector<std::string> alpha2_codes;
    for (const auto& index : selected) {
        const auto sourceIndex = proxyModel_->mapToSource(index);
        const auto* country = countryModel_->getCountry(sourceIndex.row());
        if (country)
            alpha2_codes.push_back(country->alpha2_code);
    }

    if (alpha2_codes.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "No valid countries to delete";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Delete requested for " << alpha2_codes.size()
                               << " countries";

    QString confirmMessage;
    if (alpha2_codes.size() == 1) {
        const auto sourceIndex = proxyModel_->mapToSource(selected.first());
        const auto* country = countryModel_->getCountry(sourceIndex.row());
        confirmMessage = QString("Are you sure you want to delete country '%1' (%2)?")
            .arg(QString::fromStdString(country->name))
            .arg(QString::fromStdString(country->alpha2_code));
    } else {
        confirmMessage = QString("Are you sure you want to delete %1 countries?")
            .arg(alpha2_codes.size());
    }

    auto reply =
        MessageBoxHelper::question(this, "Delete Country",
        confirmMessage, QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        BOOST_LOG_SEV(lg(), debug) << "Delete cancelled by user.";
        return;
    }

    QPointer<CountryMdiWindow> self = this;
    using DeleteResult = std::vector<std::pair<std::string, std::pair<bool, std::string>>>;

    auto task = [self, alpha2_codes]() -> DeleteResult {
        DeleteResult results;
        if (!self) return {};

        BOOST_LOG_SEV(lg(), debug) << "Making batch delete request for "
                                   << alpha2_codes.size() << " countries";

        refdata::messaging::delete_country_request request{alpha2_codes};
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            message_type::delete_country_request,
            0, std::move(payload)
        );

        auto response_result = self->clientManager_->sendRequest(
            std::move(request_frame));

        if (!response_result) {
            BOOST_LOG_SEV(lg(), error) << "Failed to send batch delete request";
            for (const auto& alpha2_code : alpha2_codes) {
                results.push_back({alpha2_code,
                    {false, "Failed to communicate with server"}});
            }
            return results;
        }

        BOOST_LOG_SEV(lg(), debug) << "Received batch delete_country_response";

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) {
            BOOST_LOG_SEV(lg(), error) << "Failed to decompress batch response";
            for (const auto& alpha2_code : alpha2_codes) {
                results.push_back({alpha2_code,
                    {false, "Failed to decompress server response"}});
            }
            return results;
        }

        auto response = refdata::messaging::delete_country_response::
            deserialize(*payload_result);

        if (!response) {
            BOOST_LOG_SEV(lg(), error) << "Failed to deserialize batch response";
            for (const auto& alpha2_code : alpha2_codes) {
                results.push_back({alpha2_code,
                    {false, "Invalid server response"}});
            }
            return results;
        }

        for (const auto& result : response->results) {
            results.push_back({result.alpha2_code,
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

        for (const auto& [alpha2_code, result] : results) {
            auto [success, message] = result;

            if (success) {
                BOOST_LOG_SEV(lg(), debug) << "Country deleted successfully: " << alpha2_code;
                success_count++;

                emit self->countryDeleted(QString::fromStdString(alpha2_code));
            } else {
                BOOST_LOG_SEV(lg(), error) << "Country deletion failed: " << alpha2_code
                                           << " - " << message;
                failure_count++;

                if (first_error.isEmpty()) {
                    first_error = QString::fromStdString(message);
                }
            }
        }

        self->countryModel_->refresh();
        if (failure_count == 0) {
            QString msg = success_count == 1
                ? "Successfully deleted 1 country"
                : QString("Successfully deleted %1 countries").arg(success_count);
            emit self->statusChanged(msg);
        } else if (success_count == 0) {
            QString msg = QString("Failed to delete %1 %2: %3")
                .arg(failure_count)
                .arg(failure_count == 1 ? "country" : "countries")
                .arg(first_error);
            emit self->errorOccurred(msg);
            MessageBoxHelper::critical(self, "Delete Failed", msg);
        } else {
            QString msg = QString("Deleted %1 %2, failed to delete %3 %4")
                .arg(success_count)
                .arg(success_count == 1 ? "country" : "countries")
                .arg(failure_count)
                .arg(failure_count == 1 ? "country" : "countries");
            emit self->statusChanged(msg);
            MessageBoxHelper::warning(self, "Partial Success", msg);
        }
    });

    QFuture<DeleteResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

void CountryMdiWindow::exportToCSV() {
    if (countryModel_->rowCount() == 0) {
        BOOST_LOG_SEV(lg(), debug) << "User requested CSV export but "
                                   << "there are no countries to export.";
        QMessageBox::information(this, "No Data",
            "There are no countries to export.");
        return;
    }

    auto countries = countryModel_->getCountries();

    QString fileName = QFileDialog::getSaveFileName(this,
        "Export to CSV",
        "countries.csv",
        "CSV Files (*.csv);;All Files (*)");

    if (fileName.isEmpty()) {
        BOOST_LOG_SEV(lg(), debug) << "User cancelled file selection in export.";
        return;
    }

    try {
        // Simple CSV export
        std::ostringstream oss;
        oss << "Alpha2,Alpha3,Numeric,Name,OfficialName,Version,ModifiedBy,RecordedAt\n";
        for (const auto& c : countries) {
            oss << c.alpha2_code << ","
                << c.alpha3_code << ","
                << c.numeric_code << ","
                << "\"" << c.name << "\","
                << "\"" << c.official_name << "\","
                << c.version << ","
                << c.modified_by << ","
                << std::chrono::duration_cast<std::chrono::seconds>(
                       c.recorded_at.time_since_epoch()).count()
                << "\n";
        }

        QFile file(fileName);
        if (!file.open(QIODevice::WriteOnly | QIODevice::Text)) {
            BOOST_LOG_SEV(lg(), error) << "Failed to open file for writing: "
                                       << fileName.toStdString();
            MessageBoxHelper::critical(this, "File Error",
                QString("Could not open file for writing: %1").arg(fileName));
            return;
        }

        auto data = oss.str();
        file.write(data.c_str(), data.length());
        file.close();

        QDesktopServices::openUrl(QUrl::fromLocalFile(fileName));

        emit statusChanged(QString("Successfully exported countries to %1")
            .arg(fileName));
        BOOST_LOG_SEV(lg(), debug) << "Successfully exported countries to CSV: "
                                   << fileName.toStdString();

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Error exporting to CSV: " << e.what();
        MessageBoxHelper::critical(this, "Export Error",
            QString("Error during CSV export: %1").arg(e.what()));
    }
}

QSize CountryMdiWindow::sizeHint() const {
    if (savedWindowSize_.isValid())
        return savedWindowSize_;

    const int minimumWidth = 900;
    const int minimumHeight = 600;

    QSize baseSize = EntityListMdiWindow::sizeHint();

    return { qMax(baseSize.width(), minimumWidth),
             qMax(baseSize.height(), minimumHeight) };
}

void CountryMdiWindow::updateActionStates() {
    const int selection_count = countryTableView_
        ->selectionModel()->selectedRows().count();
    const bool hasSelection = selection_count > 0;

    editAction_->setEnabled(hasSelection);
    deleteAction_->setEnabled(hasSelection);
    historyAction_->setEnabled(hasSelection);
}

void CountryMdiWindow::setupReloadAction() {
    reloadAction_->setIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowSync, IconUtils::DefaultIconColor));
    connect(reloadAction_, &QAction::triggered, this, &CountryMdiWindow::reload);

    initializeStaleIndicator(reloadAction_, IconUtils::iconPath(Icon::ArrowSync));
}

void CountryMdiWindow::setupColumnVisibility() {
    QHeaderView* header = countryTableView_->horizontalHeader();

    header->setContextMenuPolicy(Qt::CustomContextMenu);
    connect(header, &QHeaderView::customContextMenuRequested,
            this, &CountryMdiWindow::showHeaderContextMenu);

    connect(header, &QHeaderView::sectionMoved, this, &CountryMdiWindow::saveSettings);
    connect(header, &QHeaderView::sectionResized, this, &CountryMdiWindow::saveSettings);
}

void CountryMdiWindow::showHeaderContextMenu(const QPoint& pos) {
    QHeaderView* header = countryTableView_->horizontalHeader();
    QMenu menu(this);
    menu.setTitle(tr("Columns"));

    for (int col = 0; col < countryModel_->columnCount(); ++col) {
        QString columnName = countryModel_->headerData(col, Qt::Horizontal,
            Qt::DisplayRole).toString();

        QAction* action = menu.addAction(columnName);
        action->setCheckable(true);
        action->setChecked(!header->isSectionHidden(col));

        connect(action, &QAction::toggled, this, [this, header, col](bool visible) {
            header->setSectionHidden(col, !visible);
            saveSettings();
            BOOST_LOG_SEV(lg(), debug) << "Column " << col
                                       << " visibility changed to: " << visible;
        });
    }

    menu.exec(header->mapToGlobal(pos));
}

void CountryMdiWindow::saveSettings() {
    QSettings settings("OreStudio", "OreStudio");
    settings.beginGroup("CountryListWindow");

    QHeaderView* header = countryTableView_->horizontalHeader();
    settings.setValue("headerState", header->saveState());
    settings.setValue("windowSize", size());

    settings.endGroup();

    BOOST_LOG_SEV(lg(), trace) << "Saved country list window settings";
}

void CountryMdiWindow::restoreSettings() {
    QSettings settings("OreStudio", "OreStudio");
    settings.beginGroup("CountryListWindow");

    QHeaderView* header = countryTableView_->horizontalHeader();

    if (settings.contains("headerState")) {
        // Restore header state, falling back to defaults if corrupted
        const bool restored =
            header->restoreState(settings.value("headerState").toByteArray());
        if (restored) {
            BOOST_LOG_SEV(lg(), debug) << "Restored header state from settings";
        } else {
            BOOST_LOG_SEV(lg(), warn)
                << "Failed to restore header state, applying defaults";
            header->setSectionHidden(ClientCountryModel::NumericCode, true);
            header->setSectionHidden(ClientCountryModel::OfficialName, true);
            header->setSectionHidden(ClientCountryModel::Version, true);
            header->setSectionHidden(ClientCountryModel::ModifiedBy, true);
            header->setSectionHidden(ClientCountryModel::RecordedAt, true);
        }
    } else {
        BOOST_LOG_SEV(lg(), debug) << "No saved settings, applying default column visibility";

        // Hide these columns by default (still visible in detail view):
        // NumericCode, OfficialName, Version, ModifiedBy, RecordedAt
        header->setSectionHidden(ClientCountryModel::NumericCode, true);
        header->setSectionHidden(ClientCountryModel::OfficialName, true);
        header->setSectionHidden(ClientCountryModel::Version, true);
        header->setSectionHidden(ClientCountryModel::ModifiedBy, true);
        header->setSectionHidden(ClientCountryModel::RecordedAt, true);
    }

    if (settings.contains("windowSize")) {
        savedWindowSize_ = settings.value("windowSize").toSize();
        BOOST_LOG_SEV(lg(), debug) << "Restored window size from settings";
    }

    settings.endGroup();
}

}
