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
#include "ores.qt/ChangeReasonCategoryMdiWindow.hpp"

#include <QVBoxLayout>
#include <QHeaderView>
#include <QMessageBox>
#include <QMenu>
#include <QSettings>
#include <QtConcurrent>
#include <QFutureWatcher>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.iam/messaging/change_management_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using namespace ores::logging;

ChangeReasonCategoryMdiWindow::ChangeReasonCategoryMdiWindow(
    ClientManager* clientManager,
    const QString& username,
    QWidget* parent)
    : QWidget(parent),
      clientManager_(clientManager),
      username_(username),
      toolbar_(nullptr),
      tableView_(nullptr),
      model_(nullptr),
      proxyModel_(nullptr),
      reloadAction_(nullptr),
      addAction_(nullptr),
      editAction_(nullptr),
      deleteAction_(nullptr),
      historyAction_(nullptr),
      pulseTimer_(new QTimer(this)) {

    setupUi();
    setupConnections();

    // Initial load
    reload();
}

QSize ChangeReasonCategoryMdiWindow::sizeHint() const {
    return {800, 400};
}

void ChangeReasonCategoryMdiWindow::setupUi() {
    auto* layout = new QVBoxLayout(this);

    setupToolbar();
    layout->addWidget(toolbar_);

    setupTable();
    layout->addWidget(tableView_);
}

void ChangeReasonCategoryMdiWindow::setupToolbar() {
    toolbar_ = new QToolBar(this);
    toolbar_->setMovable(false);
    toolbar_->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    toolbar_->setIconSize(QSize(20, 20));

    const auto& iconColor = color_constants::icon_color;
    const auto& staleColor = color_constants::stale_indicator;

    normalReloadIcon_ = IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_arrow_clockwise_16_regular.svg", iconColor);
    staleReloadIcon_ = IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_arrow_clockwise_16_regular.svg", staleColor);

    reloadAction_ = toolbar_->addAction(normalReloadIcon_, tr("Reload"));
    reloadAction_->setToolTip(tr("Reload categories from server"));
    connect(reloadAction_, &QAction::triggered, this,
            &ChangeReasonCategoryMdiWindow::reload);

    toolbar_->addSeparator();

    addAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            ":/icons/ic_fluent_add_20_regular.svg", iconColor),
        tr("Add"));
    addAction_->setToolTip(tr("Add new category"));
    connect(addAction_, &QAction::triggered, this,
            &ChangeReasonCategoryMdiWindow::addNew);

    editAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            ":/icons/ic_fluent_edit_20_regular.svg", iconColor),
        tr("Edit"));
    editAction_->setToolTip(tr("Edit selected category"));
    editAction_->setEnabled(false);
    connect(editAction_, &QAction::triggered, this,
            &ChangeReasonCategoryMdiWindow::editSelected);

    deleteAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            ":/icons/ic_fluent_delete_20_regular.svg", iconColor),
        tr("Delete"));
    deleteAction_->setToolTip(tr("Delete selected category"));
    deleteAction_->setEnabled(false);
    connect(deleteAction_, &QAction::triggered, this,
            &ChangeReasonCategoryMdiWindow::deleteSelected);

    historyAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            ":/icons/ic_fluent_history_20_regular.svg", iconColor),
        tr("History"));
    historyAction_->setToolTip(tr("View category history"));
    historyAction_->setEnabled(false);
    connect(historyAction_, &QAction::triggered, this,
            &ChangeReasonCategoryMdiWindow::viewHistorySelected);

    // Pulse timer for stale indicator
    pulseTimer_->setInterval(pulse_interval_ms_);
    connect(pulseTimer_, &QTimer::timeout, this, [this]() {
        pulseState_ = !pulseState_;
        reloadAction_->setIcon(pulseState_ ? staleReloadIcon_ : normalReloadIcon_);
        pulseCount_++;
        if (pulseCount_ >= max_pulse_cycles_ * 2) {
            pulseTimer_->stop();
            reloadAction_->setIcon(staleReloadIcon_);
        }
    });
}

void ChangeReasonCategoryMdiWindow::setupTable() {
    model_ = new ClientChangeReasonCategoryModel(clientManager_, this);
    proxyModel_ = new QSortFilterProxyModel(this);
    proxyModel_->setSourceModel(model_);
    proxyModel_->setSortCaseSensitivity(Qt::CaseInsensitive);

    tableView_ = new QTableView(this);
    tableView_->setModel(proxyModel_);
    tableView_->setSelectionBehavior(QAbstractItemView::SelectRows);
    tableView_->setSelectionMode(QAbstractItemView::SingleSelection);
    tableView_->setSortingEnabled(true);
    tableView_->setAlternatingRowColors(true);
    tableView_->horizontalHeader()->setStretchLastSection(true);
    tableView_->verticalHeader()->setVisible(false);

    // Set column widths
    tableView_->setColumnWidth(ClientChangeReasonCategoryModel::Code, 250);
    tableView_->setColumnWidth(ClientChangeReasonCategoryModel::Description, 300);
    tableView_->setColumnWidth(ClientChangeReasonCategoryModel::Version, 80);
    tableView_->setColumnWidth(ClientChangeReasonCategoryModel::RecordedBy, 120);

    // Setup column visibility with context menu
    setupColumnVisibility();

    // Restore saved settings (column visibility, window size)
    restoreSettings();
}

void ChangeReasonCategoryMdiWindow::setupConnections() {
    connect(model_, &ClientChangeReasonCategoryModel::dataLoaded,
            this, &ChangeReasonCategoryMdiWindow::onDataLoaded);
    connect(model_, &ClientChangeReasonCategoryModel::loadError,
            this, &ChangeReasonCategoryMdiWindow::onLoadError);

    connect(tableView_->selectionModel(), &QItemSelectionModel::selectionChanged,
            this, &ChangeReasonCategoryMdiWindow::onSelectionChanged);
    connect(tableView_, &QTableView::doubleClicked,
            this, &ChangeReasonCategoryMdiWindow::onDoubleClicked);
}

void ChangeReasonCategoryMdiWindow::reload() {
    BOOST_LOG_SEV(lg(), debug) << "Reloading categories";
    clearStaleIndicator();
    emit statusChanged(tr("Loading categories..."));
    model_->refresh();
}

void ChangeReasonCategoryMdiWindow::onDataLoaded() {
    emit statusChanged(tr("Loaded %1 categories").arg(model_->rowCount()));
}

void ChangeReasonCategoryMdiWindow::onLoadError(const QString& error_message) {
    BOOST_LOG_SEV(lg(), error) << "Load error: " << error_message.toStdString();
    emit errorOccurred(error_message);
}

void ChangeReasonCategoryMdiWindow::onSelectionChanged() {
    updateActionStates();
}

void ChangeReasonCategoryMdiWindow::onDoubleClicked(const QModelIndex& index) {
    if (!index.isValid())
        return;

    auto sourceIndex = proxyModel_->mapToSource(index);
    if (auto* category = model_->getCategory(sourceIndex.row())) {
        emit showCategoryDetails(*category);
    }
}

void ChangeReasonCategoryMdiWindow::updateActionStates() {
    const bool hasSelection = tableView_->selectionModel()->hasSelection();
    editAction_->setEnabled(hasSelection);
    deleteAction_->setEnabled(hasSelection);
    historyAction_->setEnabled(hasSelection);
}

void ChangeReasonCategoryMdiWindow::addNew() {
    BOOST_LOG_SEV(lg(), debug) << "Add new category requested";
    emit addNewRequested();
}

void ChangeReasonCategoryMdiWindow::editSelected() {
    const auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Edit requested but no row selected";
        return;
    }

    auto sourceIndex = proxyModel_->mapToSource(selected.first());
    if (auto* category = model_->getCategory(sourceIndex.row())) {
        emit showCategoryDetails(*category);
    }
}

void ChangeReasonCategoryMdiWindow::viewHistorySelected() {
    const auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "View history requested but no row selected";
        return;
    }

    auto sourceIndex = proxyModel_->mapToSource(selected.first());
    if (auto* category = model_->getCategory(sourceIndex.row())) {
        BOOST_LOG_SEV(lg(), debug) << "Emitting showCategoryHistory for code: "
                                   << category->code;
        emit showCategoryHistory(QString::fromStdString(category->code));
    }
}

void ChangeReasonCategoryMdiWindow::deleteSelected() {
    const auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Delete requested but no row selected";
        return;
    }

    if (!clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot delete category while disconnected.");
        return;
    }

    std::vector<std::string> codes;
    for (const auto& index : selected) {
        auto sourceIndex = proxyModel_->mapToSource(index);
        if (auto* category = model_->getCategory(sourceIndex.row())) {
            codes.push_back(category->code);
        }
    }

    if (codes.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "No valid categories to delete";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Delete requested for " << codes.size()
                               << " categories";

    QString confirmMessage;
    if (codes.size() == 1) {
        confirmMessage = QString("Are you sure you want to delete category '%1'?")
            .arg(QString::fromStdString(codes.front()));
    } else {
        confirmMessage = QString("Are you sure you want to delete %1 categories?")
            .arg(codes.size());
    }

    auto reply = MessageBoxHelper::question(this, "Delete Category",
        confirmMessage, QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        BOOST_LOG_SEV(lg(), debug) << "Delete cancelled by user";
        return;
    }

    QPointer<ChangeReasonCategoryMdiWindow> self = this;
    using DeleteResult = std::vector<std::pair<std::string, std::pair<bool, std::string>>>;

    auto task = [self, codes]() -> DeleteResult {
        DeleteResult results;
        if (!self) return {};

        BOOST_LOG_SEV(lg(), debug) << "Making batch delete request for "
                                   << codes.size() << " categories";

        iam::messaging::delete_change_reason_category_request request;
        request.codes = codes;
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::delete_change_reason_category_request,
            0, std::move(payload)
        );

        auto response_result = self->clientManager_->sendRequest(
            std::move(request_frame));

        if (!response_result) {
            BOOST_LOG_SEV(lg(), error) << "Failed to send batch delete request";
            for (const auto& code : codes) {
                results.push_back({code, {false, "Failed to communicate with server"}});
            }
            return results;
        }

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) {
            BOOST_LOG_SEV(lg(), error) << "Failed to decompress batch response";
            for (const auto& code : codes) {
                results.push_back({code, {false, "Failed to decompress server response"}});
            }
            return results;
        }

        auto response = iam::messaging::delete_change_reason_category_response::
            deserialize(*payload_result);

        if (!response) {
            BOOST_LOG_SEV(lg(), error) << "Failed to deserialize batch response";
            for (const auto& code : codes) {
                results.push_back({code, {false, "Invalid server response"}});
            }
            return results;
        }

        for (const auto& result : response->results) {
            results.push_back({result.code, {result.success, result.message}});
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

        for (const auto& [code, result] : results) {
            auto [success, message] = result;

            if (success) {
                BOOST_LOG_SEV(lg(), debug) << "Category deleted: " << code;
                success_count++;
                emit self->categoryDeleted(QString::fromStdString(code));
            } else {
                BOOST_LOG_SEV(lg(), error) << "Category deletion failed: "
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
                ? "Successfully deleted 1 category"
                : QString("Successfully deleted %1 categories").arg(success_count);
            emit self->statusChanged(msg);
        } else if (success_count == 0) {
            QString msg = QString("Failed to delete %1 %2: %3")
                .arg(failure_count)
                .arg(failure_count == 1 ? "category" : "categories")
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

void ChangeReasonCategoryMdiWindow::markAsStale() {
    BOOST_LOG_SEV(lg(), debug) << "Marking as stale";
    reloadAction_->setToolTip(tr("Data changed on server - click to reload"));
    startPulseAnimation();
}

void ChangeReasonCategoryMdiWindow::clearStaleIndicator() {
    pulseTimer_->stop();
    pulseState_ = false;
    pulseCount_ = 0;
    reloadAction_->setIcon(normalReloadIcon_);
    reloadAction_->setToolTip(tr("Reload categories from server"));
}

void ChangeReasonCategoryMdiWindow::startPulseAnimation() {
    pulseCount_ = 0;
    pulseState_ = false;
    pulseTimer_->start();
}

void ChangeReasonCategoryMdiWindow::setupColumnVisibility() {
    QHeaderView* header = tableView_->horizontalHeader();

    // Enable context menu on header for column visibility
    header->setContextMenuPolicy(Qt::CustomContextMenu);
    connect(header, &QHeaderView::customContextMenuRequested,
            this, &ChangeReasonCategoryMdiWindow::showHeaderContextMenu);

    // Save header state when sections are moved or resized
    connect(header, &QHeaderView::sectionMoved, this,
            &ChangeReasonCategoryMdiWindow::saveSettings);
    connect(header, &QHeaderView::sectionResized, this,
            &ChangeReasonCategoryMdiWindow::saveSettings);
}

void ChangeReasonCategoryMdiWindow::showHeaderContextMenu(const QPoint& pos) {
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

void ChangeReasonCategoryMdiWindow::saveSettings() {
    QSettings settings("OreStudio", "OreStudio");
    settings.beginGroup("ChangeReasonCategoryListWindow");

    // Save header state (includes column visibility, order, and widths)
    QHeaderView* header = tableView_->horizontalHeader();
    settings.setValue("headerState", header->saveState());

    // Save window size
    settings.setValue("windowSize", size());

    settings.endGroup();
}

void ChangeReasonCategoryMdiWindow::restoreSettings() {
    QSettings settings("OreStudio", "OreStudio");
    settings.beginGroup("ChangeReasonCategoryListWindow");

    QHeaderView* header = tableView_->horizontalHeader();

    // Check if we have saved settings
    if (settings.contains("headerState")) {
        // Restore header state
        header->restoreState(settings.value("headerState").toByteArray());
        BOOST_LOG_SEV(lg(), debug) << "Restored header state from settings";
    } else {
        // Apply default column visibility (hide Description by default)
        BOOST_LOG_SEV(lg(), debug) << "No saved settings, applying default column visibility";
        header->setSectionHidden(ClientChangeReasonCategoryModel::Description, true);
    }

    // Restore window size if saved
    if (settings.contains("windowSize")) {
        resize(settings.value("windowSize").toSize());
    }

    settings.endGroup();
}

}
