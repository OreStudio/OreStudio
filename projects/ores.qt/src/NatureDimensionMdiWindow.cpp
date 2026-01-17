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
#include "ores.qt/NatureDimensionMdiWindow.hpp"

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
#include "ores.dq/messaging/dimension_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using namespace ores::logging;

NatureDimensionMdiWindow::NatureDimensionMdiWindow(
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
    reload();
}

QSize NatureDimensionMdiWindow::sizeHint() const {
    return {800, 400};
}

void NatureDimensionMdiWindow::setupUi() {
    auto* layout = new QVBoxLayout(this);
    setupToolbar();
    layout->addWidget(toolbar_);
    setupTable();
    layout->addWidget(tableView_);
}

void NatureDimensionMdiWindow::setupToolbar() {
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
    reloadAction_->setToolTip(tr("Reload nature dimensions from server"));
    connect(reloadAction_, &QAction::triggered, this, &NatureDimensionMdiWindow::reload);

    toolbar_->addSeparator();

    addAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(":/icons/ic_fluent_add_20_regular.svg", iconColor),
        tr("Add"));
    addAction_->setToolTip(tr("Add new nature dimension"));
    connect(addAction_, &QAction::triggered, this, &NatureDimensionMdiWindow::addNew);

    editAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(":/icons/ic_fluent_edit_20_regular.svg", iconColor),
        tr("Edit"));
    editAction_->setToolTip(tr("Edit selected nature dimension"));
    editAction_->setEnabled(false);
    connect(editAction_, &QAction::triggered, this, &NatureDimensionMdiWindow::editSelected);

    deleteAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(":/icons/ic_fluent_delete_20_regular.svg", iconColor),
        tr("Delete"));
    deleteAction_->setToolTip(tr("Delete selected nature dimension"));
    deleteAction_->setEnabled(false);
    connect(deleteAction_, &QAction::triggered, this, &NatureDimensionMdiWindow::deleteSelected);

    historyAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(":/icons/ic_fluent_history_20_regular.svg", iconColor),
        tr("History"));
    historyAction_->setToolTip(tr("View nature dimension history"));
    historyAction_->setEnabled(false);
    connect(historyAction_, &QAction::triggered, this, &NatureDimensionMdiWindow::viewHistorySelected);

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

void NatureDimensionMdiWindow::setupTable() {
    model_ = new ClientNatureDimensionModel(clientManager_, this);
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

    tableView_->setColumnWidth(ClientNatureDimensionModel::Code, 150);
    tableView_->setColumnWidth(ClientNatureDimensionModel::Name, 200);
    tableView_->setColumnWidth(ClientNatureDimensionModel::Description, 250);
    tableView_->setColumnWidth(ClientNatureDimensionModel::Version, 80);
    tableView_->setColumnWidth(ClientNatureDimensionModel::RecordedBy, 120);

    setupColumnVisibility();
    restoreSettings();
}

void NatureDimensionMdiWindow::setupConnections() {
    connect(model_, &ClientNatureDimensionModel::dataLoaded,
            this, &NatureDimensionMdiWindow::onDataLoaded);
    connect(model_, &ClientNatureDimensionModel::loadError,
            this, &NatureDimensionMdiWindow::onLoadError);
    connect(tableView_->selectionModel(), &QItemSelectionModel::selectionChanged,
            this, &NatureDimensionMdiWindow::onSelectionChanged);
    connect(tableView_, &QTableView::doubleClicked,
            this, &NatureDimensionMdiWindow::onDoubleClicked);
}

void NatureDimensionMdiWindow::reload() {
    BOOST_LOG_SEV(lg(), debug) << "Reloading nature dimensions";
    clearStaleIndicator();
    emit statusChanged(tr("Loading nature dimensions..."));
    model_->refresh();
}

void NatureDimensionMdiWindow::onDataLoaded() {
    emit statusChanged(tr("Loaded %1 nature dimensions").arg(model_->rowCount()));
}

void NatureDimensionMdiWindow::onLoadError(const QString& error_message) {
    BOOST_LOG_SEV(lg(), error) << "Load error: " << error_message.toStdString();
    emit errorOccurred(error_message);
}

void NatureDimensionMdiWindow::onSelectionChanged() {
    updateActionStates();
}

void NatureDimensionMdiWindow::onDoubleClicked(const QModelIndex& index) {
    if (!index.isValid())
        return;

    auto sourceIndex = proxyModel_->mapToSource(index);
    if (auto* dimension = model_->getDimension(sourceIndex.row())) {
        emit showDimensionDetails(*dimension);
    }
}

void NatureDimensionMdiWindow::updateActionStates() {
    const bool hasSelection = tableView_->selectionModel()->hasSelection();
    editAction_->setEnabled(hasSelection);
    deleteAction_->setEnabled(hasSelection);
    historyAction_->setEnabled(hasSelection);
}

void NatureDimensionMdiWindow::addNew() {
    BOOST_LOG_SEV(lg(), debug) << "Add new nature dimension requested";
    emit addNewRequested();
}

void NatureDimensionMdiWindow::editSelected() {
    const auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) return;

    auto sourceIndex = proxyModel_->mapToSource(selected.first());
    if (auto* dimension = model_->getDimension(sourceIndex.row())) {
        emit showDimensionDetails(*dimension);
    }
}

void NatureDimensionMdiWindow::viewHistorySelected() {
    const auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) return;

    auto sourceIndex = proxyModel_->mapToSource(selected.first());
    if (auto* dimension = model_->getDimension(sourceIndex.row())) {
        emit showDimensionHistory(QString::fromStdString(dimension->code));
    }
}

void NatureDimensionMdiWindow::deleteSelected() {
    const auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) return;

    if (!clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot delete nature dimension while disconnected.");
        return;
    }

    std::vector<std::string> codes;
    for (const auto& index : selected) {
        auto sourceIndex = proxyModel_->mapToSource(index);
        if (auto* dimension = model_->getDimension(sourceIndex.row())) {
            codes.push_back(dimension->code);
        }
    }

    if (codes.empty()) return;

    QString confirmMessage = codes.size() == 1
        ? QString("Are you sure you want to delete nature dimension '%1'?")
              .arg(QString::fromStdString(codes.front()))
        : QString("Are you sure you want to delete %1 nature dimensions?")
              .arg(codes.size());

    auto reply = MessageBoxHelper::question(this, "Delete Nature Dimension",
        confirmMessage, QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) return;

    QPointer<NatureDimensionMdiWindow> self = this;
    using DeleteResult = std::vector<std::pair<std::string, std::pair<bool, std::string>>>;

    auto task = [self, codes]() -> DeleteResult {
        DeleteResult results;
        if (!self) return {};

        dq::messaging::delete_nature_dimension_request request;
        request.codes = codes;
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::delete_nature_dimension_request,
            0, std::move(payload)
        );

        auto response_result = self->clientManager_->sendRequest(std::move(request_frame));
        if (!response_result) {
            for (const auto& code : codes) {
                results.push_back({code, {false, "Failed to communicate with server"}});
            }
            return results;
        }

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) {
            for (const auto& code : codes) {
                results.push_back({code, {false, "Failed to decompress server response"}});
            }
            return results;
        }

        auto response = dq::messaging::delete_nature_dimension_response::deserialize(*payload_result);
        if (!response) {
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
    connect(watcher, &QFutureWatcher<DeleteResult>::finished, self, [self, watcher]() {
        auto results = watcher->result();
        watcher->deleteLater();

        int success_count = 0, failure_count = 0;
        QString first_error;

        for (const auto& [code, result] : results) {
            auto [success, message] = result;
            if (success) {
                success_count++;
                emit self->dimensionDeleted(QString::fromStdString(code));
            } else {
                failure_count++;
                if (first_error.isEmpty()) first_error = QString::fromStdString(message);
            }
        }

        self->model_->refresh();

        if (failure_count == 0) {
            emit self->statusChanged(success_count == 1
                ? "Successfully deleted 1 nature dimension"
                : QString("Successfully deleted %1 nature dimensions").arg(success_count));
        } else if (success_count == 0) {
            QString msg = QString("Failed to delete: %1").arg(first_error);
            emit self->errorOccurred(msg);
            MessageBoxHelper::critical(self, "Delete Failed", msg);
        } else {
            QString msg = QString("Deleted %1, failed to delete %2").arg(success_count).arg(failure_count);
            emit self->statusChanged(msg);
            MessageBoxHelper::warning(self, "Partial Success", msg);
        }
    });

    QFuture<DeleteResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

void NatureDimensionMdiWindow::markAsStale() {
    reloadAction_->setToolTip(tr("Data changed on server - click to reload"));
    startPulseAnimation();
}

void NatureDimensionMdiWindow::clearStaleIndicator() {
    pulseTimer_->stop();
    pulseState_ = false;
    pulseCount_ = 0;
    reloadAction_->setIcon(normalReloadIcon_);
    reloadAction_->setToolTip(tr("Reload nature dimensions from server"));
}

void NatureDimensionMdiWindow::startPulseAnimation() {
    pulseCount_ = 0;
    pulseState_ = false;
    pulseTimer_->start();
}

void NatureDimensionMdiWindow::setupColumnVisibility() {
    QHeaderView* header = tableView_->horizontalHeader();
    header->setContextMenuPolicy(Qt::CustomContextMenu);
    connect(header, &QHeaderView::customContextMenuRequested,
            this, &NatureDimensionMdiWindow::showHeaderContextMenu);
    connect(header, &QHeaderView::sectionMoved, this, &NatureDimensionMdiWindow::saveSettings);
    connect(header, &QHeaderView::sectionResized, this, &NatureDimensionMdiWindow::saveSettings);
}

void NatureDimensionMdiWindow::showHeaderContextMenu(const QPoint& pos) {
    QHeaderView* header = tableView_->horizontalHeader();
    QMenu menu(this);

    for (int col = 0; col < model_->columnCount(); ++col) {
        QString columnName = model_->headerData(col, Qt::Horizontal, Qt::DisplayRole).toString();
        QAction* action = menu.addAction(columnName);
        action->setCheckable(true);
        action->setChecked(!header->isSectionHidden(col));
        connect(action, &QAction::toggled, this, [this, header, col](bool visible) {
            header->setSectionHidden(col, !visible);
            saveSettings();
        });
    }
    menu.exec(header->mapToGlobal(pos));
}

void NatureDimensionMdiWindow::saveSettings() {
    QSettings settings("OreStudio", "OreStudio");
    settings.beginGroup("NatureDimensionListWindow");
    settings.setValue("headerState", tableView_->horizontalHeader()->saveState());
    settings.setValue("windowSize", size());
    settings.endGroup();
}

void NatureDimensionMdiWindow::restoreSettings() {
    QSettings settings("OreStudio", "OreStudio");
    settings.beginGroup("NatureDimensionListWindow");

    if (settings.contains("headerState")) {
        tableView_->horizontalHeader()->restoreState(settings.value("headerState").toByteArray());
    } else {
        tableView_->horizontalHeader()->setSectionHidden(ClientNatureDimensionModel::Description, true);
    }

    if (settings.contains("windowSize")) {
        resize(settings.value("windowSize").toSize());
    }
    settings.endGroup();
}

}
