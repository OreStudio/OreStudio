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
#include "ores.qt/PartyMdiWindow.hpp"

#include <QVBoxLayout>
#include <QHeaderView>
#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <boost/uuid/uuid_io.hpp>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/EntityItemDelegate.hpp"
#include "ores.qt/BadgeColors.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ores.refdata/messaging/party_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using namespace ores::logging;

PartyMdiWindow::PartyMdiWindow(
    ClientManager* clientManager,
    ImageCache* imageCache,
    const QString& username,
    QWidget* parent)
    : EntityListMdiWindow(parent),
      clientManager_(clientManager),
      imageCache_(imageCache),
      username_(username),
      toolbar_(nullptr),
      tableView_(nullptr),
      model_(nullptr),
      proxyModel_(nullptr),
      pagination_widget_(nullptr),
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

void PartyMdiWindow::setupUi() {
    WidgetUtils::setupComboBoxes(this);
    auto* layout = new QVBoxLayout(this);

    setupToolbar();
    layout->addWidget(toolbar_);

    setupTable();
    layout->addWidget(tableView_);

    pagination_widget_ = new PaginationWidget(this);
    layout->addWidget(pagination_widget_);
}

void PartyMdiWindow::setupToolbar() {
    toolbar_ = new QToolBar(this);
    toolbar_->setMovable(false);
    toolbar_->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    toolbar_->setIconSize(QSize(20, 20));

    reloadAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::ArrowClockwise, IconUtils::DefaultIconColor),
        tr("Reload"));
    connect(reloadAction_, &QAction::triggered, this,
            &PartyMdiWindow::reload);

    initializeStaleIndicator(reloadAction_, IconUtils::iconPath(Icon::ArrowClockwise));

    toolbar_->addSeparator();

    addAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::Add, IconUtils::DefaultIconColor),
        tr("Add"));
    addAction_->setToolTip(tr("Add new party"));
    connect(addAction_, &QAction::triggered, this,
            &PartyMdiWindow::addNew);

    editAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::Edit, IconUtils::DefaultIconColor),
        tr("Edit"));
    editAction_->setToolTip(tr("Edit selected party"));
    editAction_->setEnabled(false);
    connect(editAction_, &QAction::triggered, this,
            &PartyMdiWindow::editSelected);

    deleteAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::Delete, IconUtils::DefaultIconColor),
        tr("Delete"));
    deleteAction_->setToolTip(tr("Delete selected party"));
    deleteAction_->setEnabled(false);
    connect(deleteAction_, &QAction::triggered, this,
            &PartyMdiWindow::deleteSelected);

    historyAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::History, IconUtils::DefaultIconColor),
        tr("History"));
    historyAction_->setToolTip(tr("View party history"));
    historyAction_->setEnabled(false);
    connect(historyAction_, &QAction::triggered, this,
            &PartyMdiWindow::viewHistorySelected);
}

void PartyMdiWindow::setupTable() {
    model_ = new ClientPartyModel(clientManager_, imageCache_, this);
    proxyModel_ = new QSortFilterProxyModel(this);
    proxyModel_->setSourceModel(model_);
    proxyModel_->setSortCaseSensitivity(Qt::CaseInsensitive);

    tableView_ = new QTableView(this);
    tableView_->setModel(proxyModel_);
    tableView_->setSelectionBehavior(QAbstractItemView::SelectRows);
    tableView_->setSelectionMode(QAbstractItemView::SingleSelection);
    tableView_->setSortingEnabled(true);
    using cs = column_style;
    auto* delegate = new EntityItemDelegate({
        cs::mono_bold_left, // BusinessCenterCode (flag icon inline via DecorationRole)
        cs::text_left,     // ShortCode
        cs::text_left,     // FullName
        cs::text_left,     // TransliteratedName
        cs::text_left,     // PartyCategory
        cs::text_left,     // PartyType
        cs::badge_centered, // Status
        cs::mono_center,   // Version
        cs::text_left,     // ModifiedBy
        cs::mono_left      // RecordedAt
    }, tableView_);
    delegate->set_badge_color_resolver(resolve_status_badge_color);
    tableView_->setItemDelegate(delegate);
    tableView_->setAlternatingRowColors(true);
    tableView_->verticalHeader()->setVisible(false);
    tableView_->verticalHeader()->setSectionResizeMode(QHeaderView::Fixed);

    initializeTableSettings(tableView_, model_,
        "PartyListWindow",
        {ClientPartyModel::TransliteratedName},
        {900, 400}, 4);
}

void PartyMdiWindow::setupConnections() {
    connect(model_, &ClientPartyModel::dataLoaded,
            this, &PartyMdiWindow::onDataLoaded);
    connect(model_, &ClientPartyModel::loadError,
            this, &PartyMdiWindow::onLoadError);

    connect(tableView_->selectionModel(), &QItemSelectionModel::selectionChanged,
            this, &PartyMdiWindow::onSelectionChanged);
    connect(tableView_, &QTableView::doubleClicked,
            this, &PartyMdiWindow::onDoubleClicked);

    // Connect pagination widget signals
    connect(pagination_widget_, &PaginationWidget::page_size_changed,
            this, [this](std::uint32_t size) {
        BOOST_LOG_SEV(lg(), debug) << "Page size changed to: " << size;
        model_->set_page_size(size);
        model_->refresh(true);
    });

    connect(pagination_widget_, &PaginationWidget::load_all_requested,
            this, [this]() {
        BOOST_LOG_SEV(lg(), debug) << "Load all requested from pagination widget";
        const auto total = model_->total_available_count();
        if (total > 0 && total <= 1000) {
            emit statusChanged("Loading all parties...");
            model_->set_page_size(total);
            model_->refresh(true);
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
        emit statusChanged("Loading parties...");
        model_->load_page(offset, limit);
    });
}

void PartyMdiWindow::reload() {
    BOOST_LOG_SEV(lg(), debug) << "Reloading parties";
    clearStaleIndicator();
    emit statusChanged(tr("Loading parties..."));
    model_->refresh(true);
}

void PartyMdiWindow::onDataLoaded() {
    const auto loaded = model_->rowCount();
    const auto total = model_->total_available_count();

    // Update pagination widget
    pagination_widget_->update_state(loaded, total);

    const bool has_more = loaded < static_cast<int>(total) && total > 0 && total <= 1000;
    pagination_widget_->set_load_all_enabled(has_more);

    const QString message = QString("Loaded %1 of %2 parties")
                              .arg(loaded)
                              .arg(total);
    emit statusChanged(message);
}

void PartyMdiWindow::onLoadError(const QString& error_message,
                                          const QString& details) {
    BOOST_LOG_SEV(lg(), error) << "Load error: " << error_message.toStdString();
    emit errorOccurred(error_message);
    MessageBoxHelper::critical(this, tr("Load Error"), error_message, details);
}

void PartyMdiWindow::onSelectionChanged() {
    updateActionStates();
}

void PartyMdiWindow::onDoubleClicked(const QModelIndex& index) {
    if (!index.isValid())
        return;

    auto sourceIndex = proxyModel_->mapToSource(index);
    if (auto* party = model_->getParty(sourceIndex.row())) {
        emit showPartyDetails(*party);
    }
}

void PartyMdiWindow::updateActionStates() {
    const bool hasSelection = tableView_->selectionModel()->hasSelection();
    editAction_->setEnabled(hasSelection);
    deleteAction_->setEnabled(hasSelection);
    historyAction_->setEnabled(hasSelection);
}

void PartyMdiWindow::addNew() {
    BOOST_LOG_SEV(lg(), debug) << "Add new party requested";
    emit addNewRequested();
}

void PartyMdiWindow::editSelected() {
    const auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Edit requested but no row selected";
        return;
    }

    auto sourceIndex = proxyModel_->mapToSource(selected.first());
    if (auto* party = model_->getParty(sourceIndex.row())) {
        emit showPartyDetails(*party);
    }
}

void PartyMdiWindow::viewHistorySelected() {
    const auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "View history requested but no row selected";
        return;
    }

    auto sourceIndex = proxyModel_->mapToSource(selected.first());
    if (auto* party = model_->getParty(sourceIndex.row())) {
        BOOST_LOG_SEV(lg(), debug) << "Emitting showPartyHistory for code: "
                                   << party->short_code;
        emit showPartyHistory(*party);
    }
}

void PartyMdiWindow::deleteSelected() {
    const auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Delete requested but no row selected";
        return;
    }

    if (!clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot delete party while disconnected.");
        return;
    }

    std::vector<boost::uuids::uuid> ids;
    std::vector<std::string> codes;  // For display purposes
    for (const auto& index : selected) {
        auto sourceIndex = proxyModel_->mapToSource(index);
        if (auto* party = model_->getParty(sourceIndex.row())) {
            ids.push_back(party->id);
            codes.push_back(party->short_code);
        }
    }

    if (ids.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "No valid parties to delete";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Delete requested for " << ids.size()
                               << " parties";

    QString confirmMessage;
    if (ids.size() == 1) {
        confirmMessage = QString("Are you sure you want to delete party '%1'?")
            .arg(QString::fromStdString(codes.front()));
    } else {
        confirmMessage = QString("Are you sure you want to delete %1 parties?")
            .arg(ids.size());
    }

    auto reply = MessageBoxHelper::question(this, "Delete Party",
        confirmMessage, QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        BOOST_LOG_SEV(lg(), debug) << "Delete cancelled by user";
        return;
    }

    QPointer<PartyMdiWindow> self = this;
    using DeleteResult = std::vector<std::tuple<boost::uuids::uuid, std::string, bool, std::string>>;

    auto task = [self, ids, codes]() -> DeleteResult {
        DeleteResult results;
        if (!self) return {};

        BOOST_LOG_SEV(lg(), debug) << "Making batch delete request for "
                                   << ids.size() << " parties";

        refdata::messaging::delete_party_request request;
        request.ids = ids;
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::delete_party_request,
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

        auto response = refdata::messaging::delete_party_response::
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
                BOOST_LOG_SEV(lg(), debug) << "Party deleted: " << code;
                success_count++;
                emit self->partyDeleted(QString::fromStdString(code));
            } else {
                BOOST_LOG_SEV(lg(), error) << "Party deletion failed: "
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
                ? "Successfully deleted 1 party"
                : QString("Successfully deleted %1 parties").arg(success_count);
            emit self->statusChanged(msg);
        } else if (success_count == 0) {
            QString msg = QString("Failed to delete %1 %2: %3")
                .arg(failure_count)
                .arg(failure_count == 1 ? "party" : "parties")
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
