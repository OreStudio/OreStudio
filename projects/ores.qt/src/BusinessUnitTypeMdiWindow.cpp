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
#include "ores.qt/BusinessUnitTypeMdiWindow.hpp"

#include <QVBoxLayout>
#include <QHeaderView>
#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <boost/uuid/uuid_io.hpp>
#include "ores.qt/EntityItemDelegate.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ores.refdata/messaging/business_unit_type_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using namespace ores::logging;

BusinessUnitTypeMdiWindow::BusinessUnitTypeMdiWindow(
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
      paginationWidget_(nullptr),
      reloadAction_(nullptr),
      addAction_(nullptr),
      editAction_(nullptr),
      deleteAction_(nullptr),
      historyAction_(nullptr) {

    setupUi();
    setupConnections();

    // Initial load
    reload();
}

void BusinessUnitTypeMdiWindow::setupUi() {
    WidgetUtils::setupComboBoxes(this);
    auto* layout = new QVBoxLayout(this);

    setupToolbar();
    layout->addWidget(toolbar_);

    setupTable();
    layout->addWidget(tableView_);

    paginationWidget_ = new PaginationWidget(this);
    layout->addWidget(paginationWidget_);
}

void BusinessUnitTypeMdiWindow::setupToolbar() {
    toolbar_ = new QToolBar(this);
    toolbar_->setMovable(false);
    toolbar_->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    toolbar_->setIconSize(QSize(20, 20));

    reloadAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::ArrowClockwise, IconUtils::DefaultIconColor),
        tr("Reload"));
    connect(reloadAction_, &QAction::triggered, this,
            &BusinessUnitTypeMdiWindow::reload);

    initializeStaleIndicator(reloadAction_, IconUtils::iconPath(Icon::ArrowClockwise));

    toolbar_->addSeparator();

    addAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::Add, IconUtils::DefaultIconColor),
        tr("Add"));
    addAction_->setToolTip(tr("Add new business unit type"));
    connect(addAction_, &QAction::triggered, this,
            &BusinessUnitTypeMdiWindow::addNew);

    editAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::Edit, IconUtils::DefaultIconColor),
        tr("Edit"));
    editAction_->setToolTip(tr("Edit selected business unit type"));
    editAction_->setEnabled(false);
    connect(editAction_, &QAction::triggered, this,
            &BusinessUnitTypeMdiWindow::editSelected);

    deleteAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::Delete, IconUtils::DefaultIconColor),
        tr("Delete"));
    deleteAction_->setToolTip(tr("Delete selected business unit type"));
    deleteAction_->setEnabled(false);
    connect(deleteAction_, &QAction::triggered, this,
            &BusinessUnitTypeMdiWindow::deleteSelected);

    historyAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::History, IconUtils::DefaultIconColor),
        tr("History"));
    historyAction_->setToolTip(tr("View business unit type history"));
    historyAction_->setEnabled(false);
    connect(historyAction_, &QAction::triggered, this,
            &BusinessUnitTypeMdiWindow::viewHistorySelected);
}

void BusinessUnitTypeMdiWindow::setupTable() {
    model_ = new ClientBusinessUnitTypeModel(clientManager_, this);
    proxyModel_ = new QSortFilterProxyModel(this);
    proxyModel_->setSourceModel(model_);
    proxyModel_->setSortCaseSensitivity(Qt::CaseInsensitive);

    tableView_ = new QTableView(this);
    tableView_->setModel(proxyModel_);
    tableView_->setSelectionBehavior(QAbstractItemView::SelectRows);
    tableView_->setSelectionMode(QAbstractItemView::SingleSelection);
    tableView_->setSortingEnabled(true);
    tableView_->setAlternatingRowColors(true);
    tableView_->verticalHeader()->setVisible(false);

    initializeTableSettings(tableView_, model_,
        "BusinessUnitTypeListWindow",
        {},
        {900, 400}, 2);

    using cs = column_style;
    auto* delegate = new EntityItemDelegate({
        cs::text_left,   // Code
        cs::text_left,   // Name
        cs::mono_center, // Level
        cs::text_left,   // CodingScheme
        cs::text_left,   // Description
        cs::mono_center, // Version
        cs::text_left    // ModifiedBy
    }, tableView_);
    tableView_->setItemDelegate(delegate);
}

void BusinessUnitTypeMdiWindow::setupConnections() {
    connect(model_, &ClientBusinessUnitTypeModel::dataLoaded,
            this, &BusinessUnitTypeMdiWindow::onDataLoaded);
    connect(model_, &ClientBusinessUnitTypeModel::loadError,
            this, &BusinessUnitTypeMdiWindow::onLoadError);

    connect(tableView_->selectionModel(), &QItemSelectionModel::selectionChanged,
            this, &BusinessUnitTypeMdiWindow::onSelectionChanged);
    connect(tableView_, &QTableView::doubleClicked,
            this, &BusinessUnitTypeMdiWindow::onDoubleClicked);

    connect(paginationWidget_, &PaginationWidget::page_size_changed,
            this, [this](std::uint32_t size) {
        model_->set_page_size(size);
        model_->refresh();
    });

    connect(paginationWidget_, &PaginationWidget::load_all_requested,
            this, [this]() {
        const auto total = model_->total_available_count();
        if (total > 0 && total <= 1000) {
            model_->set_page_size(total);
            model_->refresh();
        }
    });

    connect(paginationWidget_, &PaginationWidget::page_requested,
            this, [this](std::uint32_t offset, std::uint32_t limit) {
        model_->load_page(offset, limit);
    });
}

void BusinessUnitTypeMdiWindow::reload() {
    BOOST_LOG_SEV(lg(), debug) << "Reloading business unit types";
    clearStaleIndicator();
    emit statusChanged(tr("Loading business unit types..."));
    model_->refresh();
}

void BusinessUnitTypeMdiWindow::onDataLoaded() {
    const auto loaded = model_->rowCount();
    const auto total = model_->total_available_count();
    emit statusChanged(tr("Loaded %1 of %2 business unit types").arg(loaded).arg(total));

    paginationWidget_->update_state(loaded, total);
    paginationWidget_->set_load_all_enabled(
        loaded < static_cast<int>(total) && total > 0 && total <= 1000);
}

void BusinessUnitTypeMdiWindow::onLoadError(const QString& error_message,
                                             const QString& details) {
    BOOST_LOG_SEV(lg(), error) << "Load error: " << error_message.toStdString();
    emit errorOccurred(error_message);
    MessageBoxHelper::critical(this, tr("Load Error"), error_message, details);
}

void BusinessUnitTypeMdiWindow::onSelectionChanged() {
    updateActionStates();
}

void BusinessUnitTypeMdiWindow::onDoubleClicked(const QModelIndex& index) {
    if (!index.isValid())
        return;

    auto sourceIndex = proxyModel_->mapToSource(index);
    if (auto* type = model_->getType(sourceIndex.row())) {
        emit showTypeDetails(*type);
    }
}

void BusinessUnitTypeMdiWindow::updateActionStates() {
    const bool hasSelection = tableView_->selectionModel()->hasSelection();
    editAction_->setEnabled(hasSelection);
    deleteAction_->setEnabled(hasSelection);
    historyAction_->setEnabled(hasSelection);
}

void BusinessUnitTypeMdiWindow::addNew() {
    BOOST_LOG_SEV(lg(), debug) << "Add new business unit type requested";
    emit addNewRequested();
}

void BusinessUnitTypeMdiWindow::editSelected() {
    const auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Edit requested but no row selected";
        return;
    }

    auto sourceIndex = proxyModel_->mapToSource(selected.first());
    if (auto* type = model_->getType(sourceIndex.row())) {
        emit showTypeDetails(*type);
    }
}

void BusinessUnitTypeMdiWindow::viewHistorySelected() {
    const auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "View history requested but no row selected";
        return;
    }

    auto sourceIndex = proxyModel_->mapToSource(selected.first());
    if (auto* type = model_->getType(sourceIndex.row())) {
        BOOST_LOG_SEV(lg(), debug) << "Emitting showTypeHistory for code: "
                                   << type->code;
        emit showTypeHistory(*type);
    }
}

void BusinessUnitTypeMdiWindow::deleteSelected() {
    const auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Delete requested but no row selected";
        return;
    }

    if (!clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot delete business unit type while disconnected.");
        return;
    }

    std::vector<boost::uuids::uuid> ids;
    std::vector<std::string> codes;  // For display purposes
    for (const auto& index : selected) {
        auto sourceIndex = proxyModel_->mapToSource(index);
        if (auto* type = model_->getType(sourceIndex.row())) {
            ids.push_back(type->id);
            codes.push_back(type->code);
        }
    }

    if (ids.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "No valid business unit types to delete";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Delete requested for " << ids.size()
                               << " business unit types";

    QString confirmMessage;
    if (ids.size() == 1) {
        confirmMessage = QString("Are you sure you want to delete business unit type '%1'?")
            .arg(QString::fromStdString(codes.front()));
    } else {
        confirmMessage = QString("Are you sure you want to delete %1 business unit types?")
            .arg(ids.size());
    }

    auto reply = MessageBoxHelper::question(this, "Delete Business Unit Type",
        confirmMessage, QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        BOOST_LOG_SEV(lg(), debug) << "Delete cancelled by user";
        return;
    }

    QPointer<BusinessUnitTypeMdiWindow> self = this;
    using DeleteResult = std::vector<std::tuple<boost::uuids::uuid, std::string, bool, std::string>>;

    auto task = [self, ids, codes]() -> DeleteResult {
        DeleteResult results;
        if (!self) return {};

        BOOST_LOG_SEV(lg(), debug) << "Making batch delete request for "
                                   << ids.size() << " business unit types";

        refdata::messaging::delete_business_unit_type_request request;
        request.ids = ids;
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::delete_business_unit_type_request,
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

        auto response = refdata::messaging::delete_business_unit_type_response::
            deserialize(*payload_result);

        if (!response) {
            BOOST_LOG_SEV(lg(), error) << "Failed to deserialize batch response";
            for (std::size_t i = 0; i < ids.size(); ++i) {
                results.push_back({ids[i], codes[i], false, "Invalid server response"});
            }
            return results;
        }

        for (std::size_t i = 0; i < ids.size(); ++i) {
            std::string code = (i < codes.size()) ? codes[i] : "";
            results.push_back({ids[i], code, response->success, response->message});
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
                BOOST_LOG_SEV(lg(), debug) << "Business Unit Type deleted: " << code;
                success_count++;
            } else {
                BOOST_LOG_SEV(lg(), error) << "Business Unit Type deletion failed: "
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
                ? "Successfully deleted 1 business unit type"
                : QString("Successfully deleted %1 business unit types").arg(success_count);
            emit self->statusChanged(msg);
        } else if (success_count == 0) {
            QString msg = QString("Failed to delete %1 %2: %3")
                .arg(failure_count)
                .arg(failure_count == 1 ? "business unit type" : "business unit types")
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

}
