/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.qt/TenantMdiWindow.hpp"

#include <QVBoxLayout>
#include <QHeaderView>
#include <QMessageBox>
#include <QMenu>
#include <QSettings>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <boost/uuid/uuid_io.hpp>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/EntityItemDelegate.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.iam/messaging/tenant_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using namespace ores::logging;

TenantMdiWindow::TenantMdiWindow(
    ClientManager* clientManager,
    const QString& username,
    QWidget* parent)
    : EntityListMdiWindow(parent),
      clientManager_(clientManager),
      username_(username),
      toolbar_(nullptr),
      tableView_(nullptr),
      model_(nullptr),
      proxyModel_(nullptr),
      reloadAction_(nullptr),
      addAction_(nullptr),
      onboardAction_(nullptr),
      editAction_(nullptr),
      deleteAction_(nullptr),
      historyAction_(nullptr) {

    setupUi();
    setupConnections();

    // Initial load
    reload();
}

QSize TenantMdiWindow::sizeHint() const {
    return {900, 400};
}

void TenantMdiWindow::setupUi() {
    auto* layout = new QVBoxLayout(this);

    setupToolbar();
    layout->addWidget(toolbar_);

    setupTable();
    layout->addWidget(tableView_);
}

void TenantMdiWindow::setupToolbar() {
    toolbar_ = new QToolBar(this);
    toolbar_->setMovable(false);
    toolbar_->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    toolbar_->setIconSize(QSize(20, 20));

    reloadAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::ArrowClockwise, IconUtils::DefaultIconColor),
        tr("Reload"));
    connect(reloadAction_, &QAction::triggered, this,
            &TenantMdiWindow::reload);

    initializeStaleIndicator(reloadAction_, IconUtils::iconPath(Icon::ArrowClockwise));

    toolbar_->addSeparator();

    addAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::Add, IconUtils::DefaultIconColor),
        tr("Add"));
    addAction_->setToolTip(tr("Add new tenant"));
    connect(addAction_, &QAction::triggered, this,
            &TenantMdiWindow::addNew);

    onboardAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::BuildingSkyscraper, IconUtils::DefaultIconColor),
        tr("Onboard"));
    onboardAction_->setToolTip(tr("Provision a new evaluation tenant"));
    connect(onboardAction_, &QAction::triggered, this, [this]() {
        emit onboardRequested();
    });

    editAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::Edit, IconUtils::DefaultIconColor),
        tr("Edit"));
    editAction_->setToolTip(tr("Edit selected tenant"));
    editAction_->setEnabled(false);
    connect(editAction_, &QAction::triggered, this,
            &TenantMdiWindow::editSelected);

    deleteAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::Delete, IconUtils::DefaultIconColor),
        tr("Delete"));
    deleteAction_->setToolTip(tr("Delete selected tenant"));
    deleteAction_->setEnabled(false);
    connect(deleteAction_, &QAction::triggered, this,
            &TenantMdiWindow::deleteSelected);

    historyAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::History, IconUtils::DefaultIconColor),
        tr("History"));
    historyAction_->setToolTip(tr("View tenant history"));
    historyAction_->setEnabled(false);
    connect(historyAction_, &QAction::triggered, this,
            &TenantMdiWindow::viewHistorySelected);
}

void TenantMdiWindow::setupTable() {
    model_ = new ClientTenantModel(clientManager_, this);
    proxyModel_ = new QSortFilterProxyModel(this);
    proxyModel_->setSourceModel(model_);
    proxyModel_->setSortCaseSensitivity(Qt::CaseInsensitive);

    tableView_ = new QTableView(this);
    tableView_->setModel(proxyModel_);
    tableView_->setSelectionBehavior(QAbstractItemView::SelectRows);
    tableView_->setSelectionMode(QAbstractItemView::SingleSelection);
    tableView_->setSortingEnabled(true);
    using cs = column_style;
    tableView_->setItemDelegate(new EntityItemDelegate({
        cs::text_left,   // Code
        cs::text_left,   // Name
        cs::text_left,   // Type
        cs::text_left,   // Hostname
        cs::text_left,   // Status
        cs::mono_center, // Version
        cs::text_left,   // ModifiedBy
        cs::mono_left    // RecordedAt
    }, tableView_));
    tableView_->setAlternatingRowColors(true);
    tableView_->horizontalHeader()->setStretchLastSection(true);
    tableView_->verticalHeader()->setVisible(false);

    // Set column widths
    tableView_->setColumnWidth(ClientTenantModel::Code, 80);
    tableView_->setColumnWidth(ClientTenantModel::Name, 200);
    tableView_->setColumnWidth(ClientTenantModel::Type, 100);
    tableView_->setColumnWidth(ClientTenantModel::Hostname, 180);
    tableView_->setColumnWidth(ClientTenantModel::Status, 100);
    tableView_->setColumnWidth(ClientTenantModel::Version, 80);
    tableView_->setColumnWidth(ClientTenantModel::ModifiedBy, 120);
    tableView_->setColumnWidth(ClientTenantModel::RecordedAt, 150);

    // Setup column visibility with context menu
    setupColumnVisibility();

    // Restore saved settings (column visibility, window size)
    restoreSettings();
}

void TenantMdiWindow::setupConnections() {
    connect(model_, &ClientTenantModel::dataLoaded,
            this, &TenantMdiWindow::onDataLoaded);
    connect(model_, &ClientTenantModel::loadError,
            this, &TenantMdiWindow::onLoadError);

    connect(tableView_->selectionModel(), &QItemSelectionModel::selectionChanged,
            this, &TenantMdiWindow::onSelectionChanged);
    connect(tableView_, &QTableView::doubleClicked,
            this, &TenantMdiWindow::onDoubleClicked);
}

void TenantMdiWindow::reload() {
    BOOST_LOG_SEV(lg(), debug) << "Reloading tenants";
    clearStaleIndicator();
    emit statusChanged(tr("Loading tenants..."));
    model_->refresh();
}

void TenantMdiWindow::onDataLoaded() {
    emit statusChanged(tr("Loaded %1 tenants").arg(model_->rowCount()));
}

void TenantMdiWindow::onLoadError(const QString& error_message,
                                          const QString& details) {
    BOOST_LOG_SEV(lg(), error) << "Load error: " << error_message.toStdString();
    emit errorOccurred(error_message);
    MessageBoxHelper::critical(this, tr("Load Error"), error_message, details);
}

void TenantMdiWindow::onSelectionChanged() {
    updateActionStates();
}

void TenantMdiWindow::onDoubleClicked(const QModelIndex& index) {
    if (!index.isValid())
        return;

    auto sourceIndex = proxyModel_->mapToSource(index);
    if (auto* tenant = model_->getTenant(sourceIndex.row())) {
        emit showTenantDetails(*tenant);
    }
}

void TenantMdiWindow::updateActionStates() {
    const bool hasSelection = tableView_->selectionModel()->hasSelection();
    editAction_->setEnabled(hasSelection);
    deleteAction_->setEnabled(hasSelection);
    historyAction_->setEnabled(hasSelection);
}

void TenantMdiWindow::addNew() {
    BOOST_LOG_SEV(lg(), debug) << "Add new tenant requested";
    emit addNewRequested();
}

void TenantMdiWindow::editSelected() {
    const auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Edit requested but no row selected";
        return;
    }

    auto sourceIndex = proxyModel_->mapToSource(selected.first());
    if (auto* tenant = model_->getTenant(sourceIndex.row())) {
        emit showTenantDetails(*tenant);
    }
}

void TenantMdiWindow::viewHistorySelected() {
    const auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "View history requested but no row selected";
        return;
    }

    auto sourceIndex = proxyModel_->mapToSource(selected.first());
    if (auto* tenant = model_->getTenant(sourceIndex.row())) {
        BOOST_LOG_SEV(lg(), debug) << "Emitting showTenantHistory for code: "
                                   << tenant->code;
        emit showTenantHistory(*tenant);
    }
}

void TenantMdiWindow::deleteSelected() {
    const auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Delete requested but no row selected";
        return;
    }

    if (!clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot delete tenant while disconnected.");
        return;
    }

    std::vector<boost::uuids::uuid> ids;
    std::vector<std::string> codes;  // For display purposes
    for (const auto& index : selected) {
        auto sourceIndex = proxyModel_->mapToSource(index);
        if (auto* tenant = model_->getTenant(sourceIndex.row())) {
            ids.push_back(tenant->id);
            codes.push_back(tenant->code);
        }
    }

    if (ids.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "No valid tenants to delete";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Delete requested for " << ids.size()
                               << " tenants";

    QString confirmMessage;
    if (ids.size() == 1) {
        confirmMessage = QString("Are you sure you want to delete tenant '%1'?")
            .arg(QString::fromStdString(codes.front()));
    } else {
        confirmMessage = QString("Are you sure you want to delete %1 tenants?")
            .arg(ids.size());
    }

    auto reply = MessageBoxHelper::question(this, "Delete Tenant",
        confirmMessage, QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        BOOST_LOG_SEV(lg(), debug) << "Delete cancelled by user";
        return;
    }

    QPointer<TenantMdiWindow> self = this;
    using DeleteResult = std::vector<std::tuple<boost::uuids::uuid, std::string, bool, std::string>>;

    auto task = [self, ids, codes]() -> DeleteResult {
        DeleteResult results;
        if (!self) return {};

        BOOST_LOG_SEV(lg(), debug) << "Making batch delete request for "
                                   << ids.size() << " tenants";

        iam::messaging::delete_tenant_request request;
        request.ids = ids;
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::delete_tenant_request,
            0, std::move(payload)
        );

        auto response_result = self->clientManager_->sendRequest(
            std::move(request_frame));

        if (!response_result) {
            BOOST_LOG_SEV(lg(), error) << "Failed to send batch delete request";
            for (std::size_t i = 0; i < ids.size(); ++i) {
                results.push_back({ids[i], codes[i], false, "Failed to communicate with server"});
            }
            return results;
        }

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) {
            BOOST_LOG_SEV(lg(), error) << "Failed to decompress batch response";
            for (std::size_t i = 0; i < ids.size(); ++i) {
                results.push_back({ids[i], codes[i], false, "Failed to decompress server response"});
            }
            return results;
        }

        auto response = iam::messaging::delete_tenant_response::
            deserialize(*payload_result);

        if (!response) {
            BOOST_LOG_SEV(lg(), error) << "Failed to deserialize batch response";
            for (std::size_t i = 0; i < ids.size(); ++i) {
                results.push_back({ids[i], codes[i], false, "Invalid server response"});
            }
            return results;
        }

        // Match results with codes for display purposes
        for (std::size_t i = 0; i < response->results.size(); ++i) {
            const auto& result = response->results[i];
            std::string code = (i < codes.size()) ? codes[i] : "";
            results.push_back({result.id, code, result.success, result.message});
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

        for (const auto& [id, code, success, message] : results) {
            if (success) {
                BOOST_LOG_SEV(lg(), debug) << "Tenant deleted: " << code;
                success_count++;
                emit self->tenantDeleted(QString::fromStdString(code));
            } else {
                BOOST_LOG_SEV(lg(), error) << "Tenant deletion failed: "
                                           << code << " - " << message;
                failure_count++;
                if (first_error.isEmpty()) {
                    first_error = QString::fromStdString(message);
                }
            }
        }

        self->model_->refresh();

        if (failure_count == 0) {
            QString msg = success_count == 1
                ? "Successfully deleted 1 tenant"
                : QString("Successfully deleted %1 tenants").arg(success_count);
            emit self->statusChanged(msg);
        } else if (success_count == 0) {
            QString msg = QString("Failed to delete %1 %2: %3")
                .arg(failure_count)
                .arg(failure_count == 1 ? "tenant" : "tenants")
                .arg(first_error);
            emit self->errorOccurred(msg);
            MessageBoxHelper::critical(self, "Delete Failed", msg);
        } else {
            QString msg = QString("Deleted %1, failed to delete %2")
                .arg(success_count)
                .arg(failure_count);
            emit self->statusChanged(msg);
            MessageBoxHelper::warning(self, "Partial Success", msg);
        }
    });

    QFuture<DeleteResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

void TenantMdiWindow::setupColumnVisibility() {
    QHeaderView* header = tableView_->horizontalHeader();

    // Enable context menu on header for column visibility
    header->setContextMenuPolicy(Qt::CustomContextMenu);
    connect(header, &QHeaderView::customContextMenuRequested,
            this, &TenantMdiWindow::showHeaderContextMenu);

    // Save header state when sections are moved or resized
    connect(header, &QHeaderView::sectionMoved, this,
            &TenantMdiWindow::saveSettings);
    connect(header, &QHeaderView::sectionResized, this,
            &TenantMdiWindow::saveSettings);
}

void TenantMdiWindow::showHeaderContextMenu(const QPoint& pos) {
    QHeaderView* header = tableView_->horizontalHeader();
    QMenu menu(this);
    menu.setTitle(tr("Columns"));

    // Add action for each column
    for (int col = 0; col < model_->columnCount(); ++col) {
        QString columnName = model_->headerData(col, Qt::Horizontal,
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

void TenantMdiWindow::saveSettings() {
    QSettings settings("OreStudio", "OreStudio");
    settings.beginGroup("TenantListWindow");

    // Save header state (includes column visibility, order, and widths)
    QHeaderView* header = tableView_->horizontalHeader();
    settings.setValue("headerState", header->saveState());

    // Save window size
    settings.setValue("windowSize", size());

    settings.endGroup();
}

void TenantMdiWindow::restoreSettings() {
    QSettings settings("OreStudio", "OreStudio");
    settings.beginGroup("TenantListWindow");

    QHeaderView* header = tableView_->horizontalHeader();

    // Check if we have saved settings
    if (settings.contains("headerState")) {
        // Restore header state
        header->restoreState(settings.value("headerState").toByteArray());
        BOOST_LOG_SEV(lg(), debug) << "Restored header state from settings";
    } else {
        // Apply default column visibility
        BOOST_LOG_SEV(lg(), debug) << "No saved settings, applying default column visibility";
    }

    // Restore window size if saved
    if (settings.contains("windowSize")) {
        resize(settings.value("windowSize").toSize());
    }

    settings.endGroup();
}

}
