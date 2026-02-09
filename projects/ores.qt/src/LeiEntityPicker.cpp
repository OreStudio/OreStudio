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
#include "ores.qt/LeiEntityPicker.hpp"

#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QHeaderView>
#include <QPointer>
#include <QtConcurrent>
#include <QFutureWatcher>
#include "ores.dq/messaging/lei_entity_summary_protocol.hpp"
#include "ores.comms/net/client_session.hpp"

namespace ores::qt {

using namespace ores::logging;

LeiEntityPicker::LeiEntityPicker(ClientManager* clientManager,
    QWidget* parent)
    : QWidget(parent),
      clientManager_(clientManager),
      searchEdit_(nullptr),
      tableView_(nullptr),
      model_(nullptr),
      proxyModel_(nullptr),
      statusLabel_(nullptr) {
    setupUI();
}

void LeiEntityPicker::setupUI() {
    auto* mainLayout = new QVBoxLayout(this);
    mainLayout->setContentsMargins(0, 0, 0, 0);

    // Search box
    auto* searchLayout = new QHBoxLayout();
    auto* searchLabel = new QLabel("Search:", this);
    searchEdit_ = new QLineEdit(this);
    searchEdit_->setPlaceholderText("Filter by entity legal name...");
    searchEdit_->setClearButtonEnabled(true);
    searchLayout->addWidget(searchLabel);
    searchLayout->addWidget(searchEdit_);
    mainLayout->addLayout(searchLayout);

    // Table view
    model_ = new QStandardItemModel(0, 4, this);
    model_->setHorizontalHeaderLabels({"LEI", "Entity Legal Name",
        "Country", "Status"});

    proxyModel_ = new QSortFilterProxyModel(this);
    proxyModel_->setSourceModel(model_);
    proxyModel_->setFilterCaseSensitivity(Qt::CaseInsensitive);
    proxyModel_->setFilterKeyColumn(1);
    proxyModel_->setSortCaseSensitivity(Qt::CaseInsensitive);

    tableView_ = new QTableView(this);
    tableView_->setModel(proxyModel_);
    tableView_->setSelectionMode(QAbstractItemView::SingleSelection);
    tableView_->setSelectionBehavior(QAbstractItemView::SelectRows);
    tableView_->setAlternatingRowColors(true);
    tableView_->setSortingEnabled(true);
    tableView_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    tableView_->horizontalHeader()->setStretchLastSection(true);
    tableView_->verticalHeader()->setVisible(false);
    mainLayout->addWidget(tableView_);

    // Status label
    statusLabel_ = new QLabel(this);
    mainLayout->addWidget(statusLabel_);

    // Connections
    connect(searchEdit_, &QLineEdit::textChanged,
        this, &LeiEntityPicker::onSearchTextChanged);
    connect(tableView_->selectionModel(),
        &QItemSelectionModel::selectionChanged,
        this, &LeiEntityPicker::onSelectionChanged);
}

QString LeiEntityPicker::selectedLei() const {
    return selectedLei_;
}

QString LeiEntityPicker::selectedName() const {
    return selectedName_;
}

bool LeiEntityPicker::hasSelection() const {
    return !selectedLei_.isEmpty();
}

void LeiEntityPicker::load() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot load LEI entities: not connected";
        const QString msg = "Not connected to server.";
        statusLabel_->setText(msg);
        emit loadFailed(msg);
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Loading LEI entity summaries";
    statusLabel_->setText("Loading...");

    QPointer<LeiEntityPicker> self = this;

    struct LoadResult {
        bool success;
        std::string error_message;
        std::vector<dq::messaging::lei_entity_summary> entities;
    };

    auto* watcher = new QFutureWatcher<LoadResult>(this);
    connect(watcher, &QFutureWatcher<LoadResult>::finished, this,
        [self, watcher]() {
            auto result = watcher->result();
            watcher->deleteLater();

            if (!self) return;

            if (!result.success) {
                const auto msg = QString::fromStdString(result.error_message);
                BOOST_LOG_SEV(lg(), error)
                    << "Failed to load LEI entities: " << result.error_message;
                self->statusLabel_->setText("Error: " + msg);
                emit self->loadFailed(msg);
                return;
            }

            // Populate model
            self->model_->removeRows(0, self->model_->rowCount());
            for (const auto& entity : result.entities) {
                QList<QStandardItem*> row;
                row.append(new QStandardItem(
                    QString::fromStdString(entity.lei)));
                row.append(new QStandardItem(
                    QString::fromStdString(entity.entity_legal_name)));
                row.append(new QStandardItem(
                    QString::fromStdString(entity.country)));
                row.append(new QStandardItem(
                    QString::fromStdString(entity.entity_status)));
                self->model_->appendRow(row);
            }

            const int count = static_cast<int>(result.entities.size());
            self->statusLabel_->setText(
                QString("%1 entities loaded").arg(count));
            BOOST_LOG_SEV(lg(), info) << "Loaded " << count
                                      << " LEI entity summaries";
            emit self->loadCompleted(count);
        });

    QFuture<LoadResult> future = QtConcurrent::run(
        [self]() -> LoadResult {
            if (!self) return {false, "Widget destroyed", {}};

            dq::messaging::get_lei_entities_summary_request request;
            auto result = self->clientManager_->process_request(
                std::move(request));

            if (!result) {
                return {false,
                    comms::net::to_string(result.error()), {}};
            }

            if (!result->success) {
                return {false, result->error_message, {}};
            }

            return {true, {}, std::move(result->entities)};
        });

    watcher->setFuture(future);
}

void LeiEntityPicker::onSearchTextChanged(const QString& text) {
    proxyModel_->setFilterRegularExpression(
        QRegularExpression(QRegularExpression::escape(text),
            QRegularExpression::CaseInsensitiveOption));
}

void LeiEntityPicker::onSelectionChanged() {
    const auto indexes = tableView_->selectionModel()->selectedRows();
    if (indexes.isEmpty()) {
        selectedLei_.clear();
        selectedName_.clear();
        emit selectionCleared();
        return;
    }

    const auto& index = indexes.first();
    selectedLei_ = proxyModel_->data(
        proxyModel_->index(index.row(), 0)).toString();
    selectedName_ = proxyModel_->data(
        proxyModel_->index(index.row(), 1)).toString();

    BOOST_LOG_SEV(lg(), debug) << "Entity selected: "
                               << selectedLei_.toStdString()
                               << " - " << selectedName_.toStdString();

    emit entitySelected(selectedLei_, selectedName_);
}

}
