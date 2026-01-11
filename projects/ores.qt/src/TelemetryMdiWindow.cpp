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
#include "ores.qt/TelemetryMdiWindow.hpp"

#include <QtConcurrent>
#include <QFutureWatcher>
#include <QHeaderView>
#include <QDateTime>
#include <QMessageBox>
#include <boost/uuid/uuid_io.hpp>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/TelemetryLogDelegate.hpp"
#include "ores.iam/messaging/session_protocol.hpp"
#include "ores.comms/net/client_session.hpp"

namespace ores::qt {

using namespace ores::logging;

TelemetryMdiWindow::
TelemetryMdiWindow(ClientManager* clientManager,
                   const QString& username,
                   QWidget* parent)
    : QWidget(parent),
      clientManager_(clientManager),
      username_(username),
      pulseTimer_(new QTimer(this)),
      logModel_(std::make_unique<ClientTelemetryLogModel>(clientManager)) {

    BOOST_LOG_SEV(lg(), debug) << "Creating telemetry MDI window";
    setupUi();

    connect(logModel_.get(), &ClientTelemetryLogModel::dataLoaded,
            this, &TelemetryMdiWindow::onLogsLoaded);
    connect(logModel_.get(), &ClientTelemetryLogModel::loadError,
            this, &TelemetryMdiWindow::onLoadError);

    if (clientManager_) {
        connect(clientManager_, &ClientManager::connected,
                this, &TelemetryMdiWindow::onConnectionStateChanged);
        connect(clientManager_, &ClientManager::disconnected,
                this, &TelemetryMdiWindow::onConnectionStateChanged);

        // Load sessions if already connected
        if (clientManager_->isConnected()) {
            QTimer::singleShot(0, this, &TelemetryMdiWindow::reloadSessions);
        }
    }
}

TelemetryMdiWindow::~TelemetryMdiWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Destroying telemetry MDI window";
}

void TelemetryMdiWindow::setupUi() {
    auto* mainLayout = new QVBoxLayout(this);
    mainLayout->setContentsMargins(0, 0, 0, 0);

    // Main splitter: tree on left, content on right
    mainSplitter_ = new QSplitter(Qt::Horizontal, this);

    // Left panel: session tree
    leftPanel_ = new QWidget(this);
    auto* leftLayout = new QVBoxLayout(leftPanel_);
    leftLayout->setContentsMargins(5, 5, 5, 5);

    auto* sessionsLabel = new QLabel(tr("Sessions"), leftPanel_);
    sessionsLabel->setStyleSheet("font-weight: bold;");
    leftLayout->addWidget(sessionsLabel);

    setupSessionTree();
    leftLayout->addWidget(sessionTree_);

    // Right panel: toolbar + table + details
    rightPanel_ = new QWidget(this);
    rightLayout_ = new QVBoxLayout(rightPanel_);
    rightLayout_->setContentsMargins(0, 0, 0, 0);

    setupToolbar();
    setupLogTable();
    setupDetailPanels();

    mainSplitter_->addWidget(leftPanel_);
    mainSplitter_->addWidget(rightPanel_);
    mainSplitter_->setStretchFactor(0, 1);  // Tree gets 1 part
    mainSplitter_->setStretchFactor(1, 3);  // Content gets 3 parts
    mainSplitter_->setSizes({250, 750});

    mainLayout->addWidget(mainSplitter_);
}

void TelemetryMdiWindow::setupToolbar() {
    toolBar_ = new QToolBar(rightPanel_);
    toolBar_->setMovable(false);
    toolBar_->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);

    setupReloadAction();
    toolBar_->addAction(reloadAction_);

    rightLayout_->addWidget(toolBar_);
}

void TelemetryMdiWindow::setupReloadAction() {
    const auto& iconColor = color_constants::icon_color;
    const auto& staleColor = color_constants::stale_indicator;

    normalReloadIcon_ = IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_arrow_sync_20_regular.svg", iconColor);
    staleReloadIcon_ = IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_arrow_sync_20_regular.svg", staleColor);

    reloadAction_ = new QAction("Reload", this);
    reloadAction_->setIcon(normalReloadIcon_);
    reloadAction_->setToolTip(tr("Reload telemetry logs"));
    connect(reloadAction_, &QAction::triggered, this, &TelemetryMdiWindow::reload);

    connect(pulseTimer_, &QTimer::timeout, this, [this]() {
        pulseState_ = !pulseState_;
        pulseCount_++;
        reloadAction_->setIcon(pulseState_ ? staleReloadIcon_ : normalReloadIcon_);
        if (pulseCount_ >= 6) {
            stopPulseAnimation();
            reloadAction_->setIcon(staleReloadIcon_);
        }
    });
}

void TelemetryMdiWindow::setupSessionTree() {
    sessionTree_ = new QTreeWidget(leftPanel_);
    sessionTree_->setHeaderHidden(true);
    sessionTree_->setIndentation(15);

    connect(sessionTree_, &QTreeWidget::itemClicked,
            this, &TelemetryMdiWindow::onSessionSelected);
}

void TelemetryMdiWindow::setupLogTable() {
    logTableView_ = new QTableView(rightPanel_);
    logTableView_->setAlternatingRowColors(true);
    logTableView_->setSelectionMode(QAbstractItemView::SingleSelection);
    logTableView_->setSelectionBehavior(QAbstractItemView::SelectRows);
    logTableView_->setWordWrap(false);

    proxyModel_ = new QSortFilterProxyModel(this);
    proxyModel_->setSourceModel(logModel_.get());
    logTableView_->setModel(proxyModel_);
    logTableView_->setSortingEnabled(true);

    logTableView_->setItemDelegate(new TelemetryLogDelegate(logTableView_));

    QHeaderView* header = logTableView_->horizontalHeader();
    header->setSectionResizeMode(QHeaderView::Interactive);
    header->setStretchLastSection(true);

    // Set column widths
    header->resizeSection(ClientTelemetryLogModel::Timestamp, 180);
    header->resizeSection(ClientTelemetryLogModel::Level, 70);
    header->resizeSection(ClientTelemetryLogModel::Source, 150);
    header->resizeSection(ClientTelemetryLogModel::Component, 200);

    logTableView_->verticalHeader()->setSectionResizeMode(QHeaderView::Fixed);
    logTableView_->verticalHeader()->setDefaultSectionSize(24);

    connect(logTableView_->selectionModel(), &QItemSelectionModel::currentChanged,
            this, &TelemetryMdiWindow::onLogSelected);

    paginationWidget_ = new PaginationWidget(rightPanel_);
    connect(paginationWidget_, &PaginationWidget::page_requested,
            this, [this](int offset, int limit) {
        logModel_->load_page(static_cast<std::uint32_t>(offset),
                             static_cast<std::uint32_t>(limit));
    });

    rightLayout_->addWidget(logTableView_, 1);
    rightLayout_->addWidget(paginationWidget_);
}

void TelemetryMdiWindow::setupDetailPanels() {
    detailSplitter_ = new QSplitter(Qt::Vertical, rightPanel_);

    // Session info panel
    sessionInfoGroup_ = new QGroupBox(tr("Session Info"), detailSplitter_);
    auto* sessionLayout = new QHBoxLayout(sessionInfoGroup_);
    sessionLayout->setContentsMargins(10, 5, 10, 5);

    sessionUsernameLabel_ = new QLabel(sessionInfoGroup_);
    sessionClientLabel_ = new QLabel(sessionInfoGroup_);
    sessionTimeLabel_ = new QLabel(sessionInfoGroup_);
    sessionIpLabel_ = new QLabel(sessionInfoGroup_);

    sessionLayout->addWidget(sessionUsernameLabel_);
    sessionLayout->addWidget(sessionClientLabel_);
    sessionLayout->addWidget(sessionTimeLabel_);
    sessionLayout->addWidget(sessionIpLabel_);
    sessionLayout->addStretch();

    // Log detail panel
    logDetailGroup_ = new QGroupBox(tr("Log Details"), detailSplitter_);
    auto* logLayout = new QVBoxLayout(logDetailGroup_);
    logLayout->setContentsMargins(10, 5, 10, 5);

    auto* logMetaLayout = new QHBoxLayout();
    logTimestampLabel_ = new QLabel(logDetailGroup_);
    logLevelLabel_ = new QLabel(logDetailGroup_);
    logComponentLabel_ = new QLabel(logDetailGroup_);
    logMetaLayout->addWidget(logTimestampLabel_);
    logMetaLayout->addWidget(logLevelLabel_);
    logMetaLayout->addWidget(logComponentLabel_);
    logMetaLayout->addStretch();
    logLayout->addLayout(logMetaLayout);

    logMessageEdit_ = new QTextEdit(logDetailGroup_);
    logMessageEdit_->setReadOnly(true);
    logMessageEdit_->setMaximumHeight(80);
    logLayout->addWidget(logMessageEdit_);

    detailSplitter_->addWidget(sessionInfoGroup_);
    detailSplitter_->addWidget(logDetailGroup_);
    detailSplitter_->setSizes({50, 100});

    rightLayout_->addWidget(detailSplitter_);

    clearDetailPanels();
}

void TelemetryMdiWindow::reload() {
    BOOST_LOG_SEV(lg(), debug) << "Reloading telemetry data";
    clearStaleIndicator();
    reloadSessions();
}

void TelemetryMdiWindow::reloadSessions() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot reload sessions: disconnected";
        return;
    }

    loadSessions();
}

void TelemetryMdiWindow::loadSessions() {
    QPointer<TelemetryMdiWindow> self = this;

    (void)QtConcurrent::run([self]() {
        if (!self) return;

        iam::messaging::list_sessions_request request;
        request.limit = 1000;
        request.offset = 0;

        auto result = self->clientManager_->
            process_authenticated_request(std::move(request));

        if (!result) {
            BOOST_LOG_SEV(lg(), error) << "Failed to fetch sessions: "
                                       << comms::net::to_string(result.error());
            QMetaObject::invokeMethod(self, [self]() {
                emit self->errorOccurred(tr("Failed to load sessions"));
            }, Qt::QueuedConnection);
            return;
        }

        QMetaObject::invokeMethod(self, [self, sessions = std::move(result->sessions)]() {
            self->populateSessionTree(sessions);
        }, Qt::QueuedConnection);
    });
}

void TelemetryMdiWindow::
populateSessionTree(const std::vector<iam::domain::session>& sessions) {
    BOOST_LOG_SEV(lg(), debug) << "Populating session tree with "
                               << sessions.size() << " sessions";

    sessionTree_->clear();
    sessionCache_.clear();

    // Group sessions by date, then by username
    std::map<QString, std::map<QString, std::vector<const iam::domain::session*>>> grouped;

    for (const auto& session : sessions) {
        const auto msecs = std::chrono::duration_cast<std::chrono::milliseconds>(
            session.start_time.time_since_epoch()).count();
        QDateTime startTime = QDateTime::fromMSecsSinceEpoch(msecs);
        QString dateKey = startTime.toString("yyyy-MM-dd");
        QString userKey = QString::fromStdString(session.username);

        sessionCache_[boost::uuids::to_string(session.id)] = session;
        grouped[dateKey][userKey].push_back(&session);
    }

    // Build tree structure
    for (auto it = grouped.rbegin(); it != grouped.rend(); ++it) {
        const QString& date = it->first;
        auto* dateItem = new QTreeWidgetItem(sessionTree_);
        dateItem->setText(0, date);
        dateItem->setExpanded(true);

        for (const auto& [username, userSessions] : it->second) {
            auto* userItem = new QTreeWidgetItem(dateItem);
            userItem->setText(0, QString("%1 (%2 sessions)")
                .arg(username).arg(userSessions.size()));
            userItem->setExpanded(true);

            for (const auto* session : userSessions) {
                auto* sessionItem = new QTreeWidgetItem(userItem);

                const auto startMsecs = std::chrono::duration_cast<std::chrono::milliseconds>(
                    session->start_time.time_since_epoch()).count();
                QDateTime start = QDateTime::fromMSecsSinceEpoch(startMsecs);

                QString endStr = session->is_active() ? "active" :
                    QDateTime::fromMSecsSinceEpoch(
                        std::chrono::duration_cast<std::chrono::milliseconds>(
                            session->end_time->time_since_epoch()).count()
                    ).toString("hh:mm:ss");

                sessionItem->setText(0, QString("%1 - %2 (%3)")
                    .arg(start.toString("hh:mm:ss"))
                    .arg(endStr)
                    .arg(QString::fromStdString(session->client_identifier)));

                sessionItem->setData(0, Qt::UserRole,
                    QString::fromStdString(boost::uuids::to_string(session->id)));
            }
        }
    }

    emit statusChanged(tr("Loaded %1 sessions").arg(sessions.size()));
}

void TelemetryMdiWindow::onSessionSelected(QTreeWidgetItem* item, int column) {
    Q_UNUSED(column);

    QString sessionId = item->data(0, Qt::UserRole).toString();
    if (sessionId.isEmpty()) {
        return;  // Clicked on date or user node, not a session
    }

    auto it = sessionCache_.find(sessionId.toStdString());
    if (it == sessionCache_.end()) {
        return;
    }

    const auto& session = it->second;
    selectedSessionId_ = session.id;

    updateSessionInfoPanel(session);
    logModel_->load_session_logs(session.id);
}

void TelemetryMdiWindow::
updateSessionInfoPanel(const iam::domain::session& session) {
    sessionUsernameLabel_->setText(tr("User: %1")
        .arg(QString::fromStdString(session.username)));
    sessionClientLabel_->setText(tr("Client: %1 v%2.%3")
        .arg(QString::fromStdString(session.client_identifier))
        .arg(session.client_version_major)
        .arg(session.client_version_minor));

    const auto startMsecs = std::chrono::duration_cast<std::chrono::milliseconds>(
        session.start_time.time_since_epoch()).count();
    QDateTime start = QDateTime::fromMSecsSinceEpoch(startMsecs);

    if (session.is_active()) {
        sessionTimeLabel_->setText(tr("Started: %1 (active)")
            .arg(start.toString("yyyy-MM-dd hh:mm:ss")));
    } else {
        auto duration = session.duration();
        sessionTimeLabel_->setText(tr("Duration: %1 min")
            .arg(duration ? duration->count() / 60 : 0));
    }

    sessionIpLabel_->setText(tr("IP: %1")
        .arg(QString::fromStdString(session.client_ip.to_string())));
}

void TelemetryMdiWindow::
updateLogDetailPanel(const telemetry::domain::telemetry_log_entry& entry) {
    const auto msecs = std::chrono::duration_cast<std::chrono::milliseconds>(
        entry.timestamp.time_since_epoch()).count();
    QDateTime timestamp = QDateTime::fromMSecsSinceEpoch(msecs);

    logTimestampLabel_->setText(tr("Time: %1")
        .arg(timestamp.toString("yyyy-MM-dd hh:mm:ss.zzz")));
    logLevelLabel_->setText(tr("Level: %1")
        .arg(QString::fromStdString(entry.level).toUpper()));
    logComponentLabel_->setText(tr("Component: %1")
        .arg(QString::fromStdString(entry.component)));
    logMessageEdit_->setText(QString::fromStdString(entry.message));
}

void TelemetryMdiWindow::clearDetailPanels() {
    sessionUsernameLabel_->setText(tr("User: -"));
    sessionClientLabel_->setText(tr("Client: -"));
    sessionTimeLabel_->setText(tr("Time: -"));
    sessionIpLabel_->setText(tr("IP: -"));

    logTimestampLabel_->setText(tr("Time: -"));
    logLevelLabel_->setText(tr("Level: -"));
    logComponentLabel_->setText(tr("Component: -"));
    logMessageEdit_->clear();
}

void TelemetryMdiWindow::
onLogSelected(const QModelIndex& current, const QModelIndex& previous) {
    Q_UNUSED(previous);

    if (!current.isValid()) {
        return;
    }

    QModelIndex sourceIndex = proxyModel_->mapToSource(current);
    const auto* entry = logModel_->get_entry(sourceIndex.row());
    if (entry) {
        updateLogDetailPanel(*entry);
    }
}

void TelemetryMdiWindow::onLogsLoaded() {
    BOOST_LOG_SEV(lg(), debug) << "Logs loaded: " << logModel_->rowCount();

    paginationWidget_->update_state(
        static_cast<int>(logModel_->rowCount()),
        static_cast<int>(logModel_->total_available_count()));

    emit statusChanged(tr("Loaded %1 log entries").arg(logModel_->rowCount()));
}

void TelemetryMdiWindow::onLoadError(const QString& error_message) {
    BOOST_LOG_SEV(lg(), error) << "Load error: " << error_message.toStdString();
    emit errorOccurred(error_message);
}

void TelemetryMdiWindow::onConnectionStateChanged() {
    bool connected = clientManager_ && clientManager_->isConnected();
    reloadAction_->setEnabled(connected);

    if (connected) {
        reload();
    }
}

void TelemetryMdiWindow::onSessionsLoaded() {
    // Called when async session load completes
}

void TelemetryMdiWindow::markAsStale() {
    if (!isStale_) {
        isStale_ = true;
        startPulseAnimation();
        reloadAction_->setToolTip(tr("Reload (new data available)"));
    }
}

void TelemetryMdiWindow::clearStaleIndicator() {
    isStale_ = false;
    stopPulseAnimation();
    reloadAction_->setIcon(normalReloadIcon_);
    reloadAction_->setToolTip(tr("Reload telemetry logs"));
}

void TelemetryMdiWindow::startPulseAnimation() {
    pulseCount_ = 0;
    pulseState_ = true;
    pulseTimer_->start(500);
}

void TelemetryMdiWindow::stopPulseAnimation() {
    pulseTimer_->stop();
    pulseState_ = false;
    pulseCount_ = 0;
}

QSize TelemetryMdiWindow::sizeHint() const {
    return QSize(1200, 700);
}

void TelemetryMdiWindow::closeEvent(QCloseEvent* event) {
    event->accept();
}

}
