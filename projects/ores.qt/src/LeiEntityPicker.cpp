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

namespace {

/**
 * @brief Custom role for storing the LEI code on model items.
 */
constexpr int LeiRole = Qt::UserRole + 1;

/**
 * @brief Column indices for the visible table.
 */
enum Column {
    Country = 0,
    Category = 1,
    EntityLegalName = 2,
    ColumnCount = 3
};

}

LeiEntityPicker::LeiEntityPicker(ClientManager* clientManager,
    QWidget* parent)
    : QWidget(parent),
      clientManager_(clientManager),
      searchEdit_(nullptr),
      countryFilter_(nullptr),
      tableView_(nullptr),
      model_(nullptr),
      proxyModel_(nullptr),
      statusLabel_(nullptr) {
    setupUI();
}

void LeiEntityPicker::setupUI() {
    auto* mainLayout = new QVBoxLayout(this);
    mainLayout->setContentsMargins(0, 0, 0, 0);

    // Filter row: country combo + search box
    auto* filterLayout = new QHBoxLayout();

    auto* countryLabel = new QLabel("Country:", this);
    countryFilter_ = new QComboBox(this);
    countryFilter_->addItem("All Countries");
    countryFilter_->setMaxVisibleItems(10);
    countryFilter_->setSizePolicy(QSizePolicy::Preferred, QSizePolicy::Fixed);
    filterLayout->addWidget(countryLabel);
    filterLayout->addWidget(countryFilter_);

    auto* searchLabel = new QLabel("Search:", this);
    searchEdit_ = new QLineEdit(this);
    searchEdit_->setPlaceholderText("Filter by entity name...");
    searchEdit_->setClearButtonEnabled(true);
    filterLayout->addWidget(searchLabel);
    filterLayout->addWidget(searchEdit_, 1);
    mainLayout->addLayout(filterLayout);

    // Table view: Country, Category, Entity Legal Name
    model_ = new QStandardItemModel(0, Column::ColumnCount, this);
    model_->setHorizontalHeaderLabels({"Country", "Category",
        "Entity Legal Name"});

    proxyModel_ = new QSortFilterProxyModel(this);
    proxyModel_->setSourceModel(model_);
    proxyModel_->setFilterCaseSensitivity(Qt::CaseInsensitive);
    proxyModel_->setFilterKeyColumn(Column::EntityLegalName);
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
    connect(countryFilter_, &QComboBox::currentIndexChanged,
        this, &LeiEntityPicker::onCountryFilterChanged);
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

    BOOST_LOG_SEV(lg(), debug) << "Loading LEI country list";
    statusLabel_->setText("Loading countries...");

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
                    << "Failed to load LEI countries: " << result.error_message;
                self->statusLabel_->setText("Error: " + msg);
                emit self->loadFailed(msg);
                return;
            }

            // Populate country filter combo from the distinct countries.
            self->countryFilter_->blockSignals(true);
            self->countryFilter_->clear();
            self->countryFilter_->addItem("Select a country...");
            for (const auto& entity : result.entities) {
                const auto country =
                    QString::fromStdString(entity.country);
                if (!country.isEmpty())
                    self->countryFilter_->addItem(country);
            }
            self->countryFilter_->blockSignals(false);
            self->countriesLoaded_ = true;

            const int count = self->countryFilter_->count() - 1;
            self->statusLabel_->setText(
                QString("%1 countries available — select one to load entities")
                    .arg(count));
            BOOST_LOG_SEV(lg(), info) << "Loaded " << count
                                      << " LEI countries";
            emit self->loadCompleted(count);
        });

    // Request with empty country_filter → server returns distinct countries.
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

void LeiEntityPicker::loadEntitiesForCountry(const QString& country) {
    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn)
            << "Cannot load LEI entities: not connected";
        statusLabel_->setText("Not connected to server.");
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Loading LEI entities for country: "
                               << country.toStdString();
    statusLabel_->setText(QString("Loading entities for %1...").arg(country));

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
                    << "Failed to load LEI entities: "
                    << result.error_message;
                self->statusLabel_->setText("Error: " + msg);
                emit self->loadFailed(msg);
                return;
            }

            // Populate model with entities for this country.
            self->model_->removeRows(0, self->model_->rowCount());
            for (const auto& entity : result.entities) {
                auto* countryItem = new QStandardItem(
                    QString::fromStdString(entity.country));
                countryItem->setData(
                    QString::fromStdString(entity.lei), LeiRole);
                auto* categoryItem = new QStandardItem(
                    QString::fromStdString(entity.entity_category));
                auto* nameItem = new QStandardItem(
                    QString::fromStdString(entity.entity_legal_name));

                self->model_->appendRow(
                    {countryItem, categoryItem, nameItem});
            }

            const int count = static_cast<int>(result.entities.size());
            self->statusLabel_->setText(
                QString("%1 entities loaded").arg(count));
            BOOST_LOG_SEV(lg(), info) << "Loaded " << count
                                      << " LEI entities for country";
            self->applyFilters();
        });

    const auto countryStd = country.toStdString();
    QFuture<LoadResult> future = QtConcurrent::run(
        [self, countryStd]() -> LoadResult {
            if (!self) return {false, "Widget destroyed", {}};

            dq::messaging::get_lei_entities_summary_request request;
            request.country_filter = countryStd;
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

void LeiEntityPicker::onSearchTextChanged(const QString& /*text*/) {
    applyFilters();
}

void LeiEntityPicker::onCountryFilterChanged(int index) {
    if (!countriesLoaded_)
        return;

    if (index > 0) {
        const auto country = countryFilter_->currentText();
        loadEntitiesForCountry(country);
    } else {
        // "Select a country..." re-selected — clear the table.
        model_->removeRows(0, model_->rowCount());
        selectedLei_.clear();
        selectedName_.clear();
        statusLabel_->setText("Select a country to load entities");
        emit selectionCleared();
    }
}

void LeiEntityPicker::applyFilters() {
    const auto searchText = searchEdit_->text();
    proxyModel_->setFilterKeyColumn(Column::EntityLegalName);
    proxyModel_->setFilterRegularExpression(
        QRegularExpression(QRegularExpression::escape(searchText),
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
    // LEI is stored as UserRole data on the Country column item.
    selectedLei_ = proxyModel_->data(
        proxyModel_->index(index.row(), Column::Country), LeiRole).toString();
    selectedName_ = proxyModel_->data(
        proxyModel_->index(index.row(), Column::EntityLegalName)).toString();

    BOOST_LOG_SEV(lg(), debug) << "Entity selected: "
                               << selectedLei_.toStdString()
                               << " - " << selectedName_.toStdString();

    emit entitySelected(selectedLei_, selectedName_);
}

}
