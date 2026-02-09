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

#include <set>
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

            // Collect distinct countries for the filter combo.
            std::set<QString> countries;

            // Populate model
            self->model_->removeRows(0, self->model_->rowCount());
            for (const auto& entity : result.entities) {
                const auto country =
                    QString::fromStdString(entity.country);
                countries.insert(country);

                auto* countryItem = new QStandardItem(country);
                countryItem->setData(
                    QString::fromStdString(entity.lei), LeiRole);
                auto* categoryItem = new QStandardItem(
                    QString::fromStdString(entity.entity_category));
                auto* nameItem = new QStandardItem(
                    QString::fromStdString(entity.entity_legal_name));

                self->model_->appendRow(
                    {countryItem, categoryItem, nameItem});
            }

            // Populate country filter combo (preserve current selection).
            const auto current = self->countryFilter_->currentText();
            self->countryFilter_->blockSignals(true);
            self->countryFilter_->clear();
            self->countryFilter_->addItem("All Countries");
            for (const auto& c : countries)
                self->countryFilter_->addItem(c);

            const int idx = self->countryFilter_->findText(current);
            self->countryFilter_->setCurrentIndex(idx > 0 ? idx : 0);
            self->countryFilter_->blockSignals(false);

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

void LeiEntityPicker::onSearchTextChanged(const QString& /*text*/) {
    applyFilters();
}

void LeiEntityPicker::onCountryFilterChanged(int /*index*/) {
    applyFilters();
}

void LeiEntityPicker::applyFilters() {
    const auto searchText = searchEdit_->text();
    const auto country = countryFilter_->currentText();
    const bool filterByCountry =
        (countryFilter_->currentIndex() > 0);

    // Build a regex that matches the search text on the name column.
    proxyModel_->setFilterKeyColumn(Column::EntityLegalName);
    proxyModel_->setFilterRegularExpression(
        QRegularExpression(QRegularExpression::escape(searchText),
            QRegularExpression::CaseInsensitiveOption));

    // For country filtering, iterate source rows and hide/show.
    // QSortFilterProxyModel only supports one filter column natively,
    // so we set row visibility via a custom filter approach:
    // Override isn't possible after construction, so we manually hide
    // rows that don't match the country filter.
    if (filterByCountry) {
        for (int row = 0; row < proxyModel_->rowCount(); ++row) {
            const auto proxyIndex = proxyModel_->index(row, Column::Country);
            const auto rowCountry = proxyModel_->data(proxyIndex).toString();
            tableView_->setRowHidden(row, rowCountry != country);
        }
    } else {
        for (int row = 0; row < proxyModel_->rowCount(); ++row) {
            tableView_->setRowHidden(row, false);
        }
    }
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
