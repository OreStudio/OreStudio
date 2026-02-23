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
#include "ores.qt/PublicationHistoryDialog.hpp"
#include "ores.qt/WidgetUtils.hpp"

#include <QDateTime>
#include <QDialogButtonBox>
#include <QHeaderView>
#include <QLabel>
#include <QPushButton>
#include <QtConcurrent>
#include <boost/uuid/uuid_io.hpp>
#include "ores.dq/domain/publication_mode.hpp"
#include "ores.dq/messaging/publication_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

// PublicationHistoryModel implementation

PublicationHistoryModel::PublicationHistoryModel(QObject* parent)
    : QAbstractTableModel(parent) {}

int PublicationHistoryModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid()) return 0;
    return static_cast<int>(publications_.size());
}

int PublicationHistoryModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid()) return 0;
    return ColumnCount;
}

QVariant PublicationHistoryModel::data(const QModelIndex& index, int role) const {
    if (!index.isValid() || index.row() >= static_cast<int>(publications_.size()))
        return QVariant();

    const auto& pub = publications_[index.row()];

    if (role == Qt::DisplayRole) {
        switch (static_cast<Column>(index.column())) {
        case PublishedAt: {
            auto qdt = QDateTime::fromSecsSinceEpoch(
                std::chrono::system_clock::to_time_t(pub.published_at));
            return qdt.toString("yyyy-MM-dd hh:mm:ss");
        }
        case DatasetCode:
            return QString::fromStdString(pub.dataset_code);
        case Mode:
            return QString::fromStdString(
                dq::domain::to_string(pub.mode));
        case TargetTable:
            return QString::fromStdString(pub.target_table);
        case Inserted:
            return QString::number(pub.records_inserted);
        case Skipped:
            return QString::number(pub.records_skipped);
        case Deleted:
            return QString::number(pub.records_deleted);
        case PublishedBy:
            return QString::fromStdString(pub.published_by);
        default:
            return QVariant();
        }
    }

    if (role == Qt::TextAlignmentRole) {
        switch (static_cast<Column>(index.column())) {
        case Inserted:
        case Skipped:
        case Deleted:
            return Qt::AlignRight;
        default:
            return QVariant();
        }
    }

    return QVariant();
}

QVariant PublicationHistoryModel::headerData(int section, Qt::Orientation orientation,
    int role) const {
    if (orientation != Qt::Horizontal || role != Qt::DisplayRole)
        return QVariant();

    switch (static_cast<Column>(section)) {
    case PublishedAt:   return tr("Published At");
    case DatasetCode:   return tr("Dataset");
    case Mode:          return tr("Mode");
    case TargetTable:   return tr("Target Table");
    case Inserted:      return tr("Inserted");
    case Skipped:       return tr("Skipped");
    case Deleted:       return tr("Deleted");
    case PublishedBy:   return tr("Published By");
    default:            return QVariant();
    }
}

void PublicationHistoryModel::setPublications(
    const std::vector<dq::domain::publication>& publications) {
    beginResetModel();
    publications_ = publications;
    endResetModel();
}

void PublicationHistoryModel::clear() {
    beginResetModel();
    publications_.clear();
    endResetModel();
}

// PublicationHistoryDialog implementation

PublicationHistoryDialog::PublicationHistoryDialog(ClientManager* clientManager,
                                                   QWidget* parent)
    : QDialog(parent),
      clientManager_(clientManager),
      watcher_(new QFutureWatcher<FetchResult>(this)) {
    setupUi();

    connect(watcher_, &QFutureWatcher<FetchResult>::finished,
            this, &PublicationHistoryDialog::onPublicationsLoaded);
}

PublicationHistoryDialog::~PublicationHistoryDialog() = default;

void PublicationHistoryDialog::setupUi() {
    WidgetUtils::setupComboBoxes(this);
    setWindowTitle(tr("Publication History"));
    setMinimumSize(900, 500);

    auto* layout = new QVBoxLayout(this);

    // Table view
    tableView_ = new QTableView(this);
    model_ = new PublicationHistoryModel(this);
    tableView_->setModel(model_);
    tableView_->setSelectionBehavior(QAbstractItemView::SelectRows);
    tableView_->setSelectionMode(QAbstractItemView::SingleSelection);
    tableView_->setAlternatingRowColors(true);
    tableView_->horizontalHeader()->setStretchLastSection(true);
    tableView_->verticalHeader()->setVisible(false);
    tableView_->setSortingEnabled(true);
    layout->addWidget(tableView_);

    // Button box
    auto* buttonBox = new QDialogButtonBox(this);
    auto* refreshButton = buttonBox->addButton(tr("Refresh"),
        QDialogButtonBox::ActionRole);
    buttonBox->addButton(QDialogButtonBox::Close);

    connect(refreshButton, &QPushButton::clicked, this, &PublicationHistoryDialog::refresh);
    connect(buttonBox, &QDialogButtonBox::rejected, this, &QDialog::reject);

    layout->addWidget(buttonBox);
}

void PublicationHistoryDialog::refresh() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot refresh: not connected to server";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Fetching publication history";

    auto future = QtConcurrent::run([this]() -> FetchResult {
        try {
            dq::messaging::get_publications_request request;
            // Empty dataset_id (nil UUID) = get recent publications across all datasets
            request.limit = 100;

            auto result = clientManager_->process_request(std::move(request));
            if (result) {
                return FetchResult{
                    .success = true,
                    .publications = std::move(result->publications)
                };
            }
            return FetchResult{.success = false};
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "Failed to fetch publications: " << e.what();
            return FetchResult{.success = false};
        }
    });

    watcher_->setFuture(future);
}

void PublicationHistoryDialog::onPublicationsLoaded() {
    auto result = watcher_->result();

    if (result.success) {
        model_->setPublications(result.publications);

        // Resize columns to content
        for (int i = 0; i < model_->columnCount(); ++i) {
            tableView_->resizeColumnToContents(i);
        }

        BOOST_LOG_SEV(lg(), debug) << "Loaded " << result.publications.size()
                                   << " publication records";
        emit statusMessage(tr("Loaded %1 publication records").arg(result.publications.size()));
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Failed to load publications";
        emit errorMessage(tr("Failed to load publication history"));
    }
}

}
