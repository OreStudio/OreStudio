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
#include "ores.qt/SessionHistoryDialog.hpp"

#include <algorithm>
#include <QDateTime>
#include <QHBoxLayout>
#include <QHeaderView>
#include <QLabel>
#include <QPainter>
#include <QPushButton>
#include <QtConcurrent>
#include <QtCharts/QChart>
#include <QtCharts/QLineSeries>
#include <QtCharts/QDateTimeAxis>
#include <QtCharts/QValueAxis>
#include <boost/uuid/uuid_io.hpp>
#include "ores.comms/service/auth_session_service.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"

namespace ores::qt {

using namespace ores::logging;

// SessionHistoryModel implementation

SessionHistoryModel::SessionHistoryModel(QObject* parent)
    : QAbstractTableModel(parent) {}

int SessionHistoryModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid()) return 0;
    return static_cast<int>(sessions_.size());
}

int SessionHistoryModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid()) return 0;
    return ColumnCount;
}

QVariant SessionHistoryModel::data(const QModelIndex& index, int role) const {
    if (!index.isValid() || index.row() >= static_cast<int>(sessions_.size()))
        return QVariant();

    const auto& session = sessions_[index.row()];

    if (role == Qt::DisplayRole) {
        switch (static_cast<Column>(index.column())) {
        case StartTime: {
            auto qdt = QDateTime::fromSecsSinceEpoch(
                std::chrono::system_clock::to_time_t(session.start_time));
            return qdt.toString("yyyy-MM-dd hh:mm:ss");
        }
        case EndTime: {
            if (session.end_time) {
                auto qdt = QDateTime::fromSecsSinceEpoch(
                    std::chrono::system_clock::to_time_t(*session.end_time));
                return qdt.toString("yyyy-MM-dd hh:mm:ss");
            }
            return tr("Active");
        }
        case Duration: {
            if (auto dur = session.duration()) {
                auto minutes = std::chrono::duration_cast<std::chrono::minutes>(*dur);
                auto hours = std::chrono::duration_cast<std::chrono::hours>(minutes);
                minutes -= std::chrono::duration_cast<std::chrono::minutes>(hours);
                if (hours.count() > 0) {
                    return QString("%1h %2m").arg(hours.count()).arg(minutes.count());
                }
                return QString("%1m").arg(minutes.count());
            }
            return tr("Ongoing");
        }
        case ClientIP:
            return QString::fromStdString(session.client_ip.to_string());
        case Country:
            return QString::fromStdString(session.country_code);
        case BytesSent:
            if (!session.end_time) return QString("-");
            if (session.bytes_sent >= 1024 * 1024) {
                return QString("%1 MB").arg(session.bytes_sent / (1024.0 * 1024.0), 0, 'f', 2);
            } else if (session.bytes_sent >= 1024) {
                return QString("%1 KB").arg(session.bytes_sent / 1024.0, 0, 'f', 2);
            }
            return QString("%1 B").arg(session.bytes_sent);
        case BytesReceived:
            if (!session.end_time) return QString("-");
            if (session.bytes_received >= 1024 * 1024) {
                return QString("%1 MB").arg(session.bytes_received / (1024.0 * 1024.0), 0, 'f', 2);
            } else if (session.bytes_received >= 1024) {
                return QString("%1 KB").arg(session.bytes_received / 1024.0, 0, 'f', 2);
            }
            return QString("%1 B").arg(session.bytes_received);
        case ClientVersion:
            if (session.client_version_major > 0 || session.client_version_minor > 0) {
                return QString("%1.%2").arg(session.client_version_major)
                                       .arg(session.client_version_minor);
            }
            return QString("-");
        default:
            return QVariant();
        }
    }

    if (role == Qt::TextAlignmentRole) {
        switch (static_cast<Column>(index.column())) {
        case BytesSent:
        case BytesReceived:
            return Qt::AlignRight;
        default:
            return QVariant();
        }
    }

    if (role == Qt::ForegroundRole) {
        if (!session.end_time) {
            // Active session - show in green
            return QColor(0, 150, 0);
        }
    }

    return QVariant();
}

QVariant SessionHistoryModel::headerData(int section, Qt::Orientation orientation,
    int role) const {
    if (orientation != Qt::Horizontal || role != Qt::DisplayRole)
        return QVariant();

    switch (static_cast<Column>(section)) {
    case StartTime:     return tr("Start Time");
    case EndTime:       return tr("End Time");
    case Duration:      return tr("Duration");
    case ClientIP:      return tr("IP Address");
    case Country:       return tr("Country");
    case BytesSent:     return tr("Sent");
    case BytesReceived: return tr("Received");
    case ClientVersion: return tr("Version");
    default:            return QVariant();
    }
}

void SessionHistoryModel::setSessions(const std::vector<iam::domain::session>& sessions) {
    beginResetModel();
    sessions_ = sessions;
    endResetModel();
}

void SessionHistoryModel::clear() {
    beginResetModel();
    sessions_.clear();
    endResetModel();
}

// SessionHistoryDialog implementation

SessionHistoryDialog::SessionHistoryDialog(ClientManager* clientManager,
                                           QWidget* parent)
    : QWidget(parent),
      clientManager_(clientManager),
      watcher_(new QFutureWatcher<FetchResult>(this)),
      samplesWatcher_(new QFutureWatcher<FetchSamplesResult>(this)) {
    setupUi();

    connect(watcher_, &QFutureWatcher<FetchResult>::finished,
            this, &SessionHistoryDialog::onSessionsLoaded);
    connect(samplesWatcher_, &QFutureWatcher<FetchSamplesResult>::finished,
            this, &SessionHistoryDialog::onSamplesLoaded);
}

SessionHistoryDialog::~SessionHistoryDialog() = default;

void SessionHistoryDialog::setupUi() {
    setWindowTitle(tr("Session History"));
    setMinimumSize(900, 600);

    auto* layout = new QVBoxLayout(this);

    splitter_ = new QSplitter(Qt::Vertical, this);

    // Table view (top pane)
    tableView_ = new QTableView(this);
    model_ = new SessionHistoryModel(this);
    tableView_->setModel(model_);
    tableView_->setSelectionBehavior(QAbstractItemView::SelectRows);
    tableView_->setSelectionMode(QAbstractItemView::SingleSelection);
    tableView_->setAlternatingRowColors(true);
    tableView_->horizontalHeader()->setStretchLastSection(true);
    tableView_->verticalHeader()->setVisible(false);
    splitter_->addWidget(tableView_);

    // Chart view (bottom pane)
    auto* chart = new QChart();
    chart->setTitle(tr("Select a session to view bytes over time"));
    chart->legend()->setVisible(true);
    chartView_ = new QChartView(chart, this);
    chartView_->setRenderHint(QPainter::Antialiasing);
    chartView_->setMinimumHeight(200);
    splitter_->addWidget(chartView_);

    splitter_->setStretchFactor(0, 2);
    splitter_->setStretchFactor(1, 1);

    layout->addWidget(splitter_);

    // Connect selection signal
    connect(tableView_->selectionModel(), &QItemSelectionModel::selectionChanged,
            this, &SessionHistoryDialog::onSessionSelectionChanged);

    // Button bar
    auto* refreshButton = new QPushButton(tr("Refresh"), this);
    connect(refreshButton, &QPushButton::clicked, this, &SessionHistoryDialog::refresh);

    auto* buttonLayout = new QHBoxLayout;
    buttonLayout->addStretch();
    buttonLayout->addWidget(refreshButton);
    layout->addLayout(buttonLayout);
}

void SessionHistoryDialog::setAccount(const boost::uuids::uuid& accountId,
                                      const QString& username) {
    accountId_ = accountId;
    username_ = username;
    setWindowTitle(tr("Session History - %1").arg(username));
    refresh();
}

void SessionHistoryDialog::refresh() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot refresh: not connected to server";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Fetching sessions for account: "
                               << boost::uuids::to_string(accountId_);

    auto future = QtConcurrent::run([this]() -> FetchResult {
        try {
            // Request sessions for this account
            auto result = clientManager_->listSessions(accountId_);
            if (result) {
                return FetchResult{
                    .success = true,
                    .sessions = std::move(result->sessions),
                    .total_count = result->total_count
                };
            }
            return FetchResult{.success = false};
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "Failed to fetch sessions: " << e.what();
            return FetchResult{.success = false};
        }
    });

    watcher_->setFuture(future);
}

void SessionHistoryDialog::onSessionsLoaded() {
    auto result = watcher_->result();

    if (result.success) {
        model_->setSessions(result.sessions);

        // Resize columns to content
        for (int i = 0; i < model_->columnCount(); ++i) {
            tableView_->resizeColumnToContents(i);
        }

        BOOST_LOG_SEV(lg(), debug) << "Loaded " << result.sessions.size()
                                   << " sessions (total: " << result.total_count << ")";
        emit statusMessage(tr("Loaded %1 sessions").arg(result.sessions.size()));
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Failed to load sessions";
        emit errorMessage(tr("Failed to load session history"));
    }
}

void SessionHistoryDialog::onSessionSelectionChanged(
    const QItemSelection& selected, const QItemSelection&) {

    if (selected.isEmpty()) return;

    const int row = selected.indexes().first().row();
    if (row < 0 || row >= static_cast<int>(model_->sessions().size())) return;

    const auto& session = model_->sessions()[static_cast<std::size_t>(row)];
    const auto session_id = session.id;
    const bool is_active = !session.end_time.has_value();
    auto qdt = QDateTime::fromSecsSinceEpoch(
        std::chrono::system_clock::to_time_t(session.start_time));
    const QString label = qdt.toString("yyyy-MM-dd hh:mm:ss");

    // Show loading state in chart title
    chartView_->chart()->setTitle(tr("Loading samples for session: %1").arg(label));

    auto future = QtConcurrent::run([this, session_id, label, is_active]() -> FetchSamplesResult {
        try {
            auto samples = clientManager_->getSessionSamples(session_id);
            if (samples) {
                return FetchSamplesResult{
                    .success = true,
                    .is_active = is_active,
                    .session_id = session_id,
                    .session_label = label,
                    .samples = std::move(*samples)
                };
            }
            return FetchSamplesResult{.success = false, .is_active = is_active,
                                      .session_id = session_id, .session_label = label};
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "Failed to fetch samples: " << e.what();
            return FetchSamplesResult{.success = false, .is_active = is_active,
                                      .session_id = session_id, .session_label = label};
        }
    });

    samplesWatcher_->setFuture(future);
}

void SessionHistoryDialog::onSamplesLoaded() {
    auto result = samplesWatcher_->result();

    auto* chart = chartView_->chart();
    chart->removeAllSeries();

    // Remove any existing axes
    const auto axes = chart->axes();
    for (auto* axis : axes) {
        chart->removeAxis(axis);
    }

    if (!result.success || result.samples.empty()) {
        if (!result.success) {
            chart->setTitle(tr("Failed to load samples for: %1").arg(result.session_label));
        } else if (result.is_active) {
            chart->setTitle(tr("Session in progress — no data yet (samples flush every %1 heartbeats)")
                .arg(comms::service::auth_session_service::sample_flush_interval));
        } else {
            chart->setTitle(tr("Session: %1 (no samples recorded)").arg(result.session_label));
        }
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Displaying " << result.samples.size()
                               << " samples for session " << result.session_label.toStdString();

    auto* sent_series = new QLineSeries();
    sent_series->setName(tr("Sent"));
    auto* recv_series = new QLineSeries();
    recv_series->setName(tr("Received"));

    qreal max_bytes = 0;
    for (const auto& s : result.samples) {
        const qreal t = static_cast<qreal>(s.sample_time_ms);
        const qreal bs = static_cast<qreal>(s.bytes_sent);
        const qreal br = static_cast<qreal>(s.bytes_received);
        sent_series->append(t, bs);
        recv_series->append(t, br);
        max_bytes = std::max({max_bytes, bs, br});
    }

    chart->addSeries(sent_series);
    chart->addSeries(recv_series);

    // X axis: date-time
    auto* x_axis = new QDateTimeAxis();
    x_axis->setFormat("hh:mm:ss");
    x_axis->setTitleText(tr("Time"));
    chart->addAxis(x_axis, Qt::AlignBottom);
    sent_series->attachAxis(x_axis);
    recv_series->attachAxis(x_axis);

    // Y axis: bytes
    auto* y_axis = new QValueAxis();
    y_axis->setTitleText(tr("Bytes"));
    y_axis->setRange(0, max_bytes * 1.05);
    chart->addAxis(y_axis, Qt::AlignLeft);
    sent_series->attachAxis(y_axis);
    recv_series->attachAxis(y_axis);

    chart->setTitle(result.is_active
        ? tr("Session: %1 (in progress — partial data)").arg(result.session_label)
        : tr("Session: %1").arg(result.session_label));
    chart->legend()->setVisible(true);

    // Colour the series
    sent_series->setColor(QColor(30, 144, 255));   // dodger blue
    recv_series->setColor(QColor(50, 205, 50));    // lime green
}

}
