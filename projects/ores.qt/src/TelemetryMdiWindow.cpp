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

#include <algorithm>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <QHeaderView>
#include <QDateTime>
#include <QBrush>
#include "ores.qt/MessageBoxHelper.hpp"
#include <QRegularExpression>
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

    // Main splitter: tree+details on left, logs on right
    mainSplitter_ = new QSplitter(Qt::Horizontal, this);

    // Left panel: session tree + session details (vertical splitter)
    leftPanel_ = new QWidget(this);
    auto* leftLayout = new QVBoxLayout(leftPanel_);
    leftLayout->setContentsMargins(5, 5, 5, 5);

    leftSplitter_ = new QSplitter(Qt::Vertical, leftPanel_);

    // Session tree
    auto* treeContainer = new QWidget(leftSplitter_);
    auto* treeLayout = new QVBoxLayout(treeContainer);
    treeLayout->setContentsMargins(0, 0, 0, 0);

    auto* sessionsLabel = new QLabel(tr("Sessions"), treeContainer);
    sessionsLabel->setStyleSheet("font-weight: bold;");
    treeLayout->addWidget(sessionsLabel);

    setupSessionTree();
    treeLayout->addWidget(sessionTree_);

    // Session details table
    sessionDetailsGroup_ = new QGroupBox(tr("Session Details"), leftSplitter_);
    auto* detailsLayout = new QVBoxLayout(sessionDetailsGroup_);
    detailsLayout->setContentsMargins(5, 5, 5, 5);

    sessionDetailsTable_ = new QTableWidget(sessionDetailsGroup_);
    sessionDetailsTable_->setColumnCount(2);
    sessionDetailsTable_->setHorizontalHeaderLabels({tr("Field"), tr("Value")});
    sessionDetailsTable_->horizontalHeader()->setStretchLastSection(true);
    sessionDetailsTable_->horizontalHeader()->setSectionResizeMode(0, QHeaderView::ResizeToContents);
    sessionDetailsTable_->verticalHeader()->setVisible(false);
    sessionDetailsTable_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    sessionDetailsTable_->setSelectionMode(QAbstractItemView::NoSelection);
    sessionDetailsTable_->setAlternatingRowColors(true);
    detailsLayout->addWidget(sessionDetailsTable_);

    leftSplitter_->addWidget(treeContainer);
    leftSplitter_->addWidget(sessionDetailsGroup_);
    leftSplitter_->setStretchFactor(0, 2);  // Tree gets more space
    leftSplitter_->setStretchFactor(1, 1);  // Details gets less

    leftLayout->addWidget(leftSplitter_);

    // Right panel: toolbar + table + details
    rightPanel_ = new QWidget(this);
    rightLayout_ = new QVBoxLayout(rightPanel_);
    rightLayout_->setContentsMargins(0, 0, 0, 0);

    setupToolbar();
    setupLogTable();
    setupDetailPanels();

    mainSplitter_->addWidget(leftPanel_);
    mainSplitter_->addWidget(rightPanel_);
    mainSplitter_->setStretchFactor(0, 1);  // Left panel gets 1 part
    mainSplitter_->setStretchFactor(1, 3);  // Right panel gets 3 parts
    mainSplitter_->setSizes({300, 900});

    mainLayout->addWidget(mainSplitter_);
}

void TelemetryMdiWindow::setupToolbar() {
    toolBar_ = new QToolBar(rightPanel_);
    toolBar_->setMovable(false);
    toolBar_->setToolButtonStyle(Qt::ToolButtonIconOnly);

    setupReloadAction();
    toolBar_->addAction(reloadAction_);

    toolBar_->addSeparator();
    setupLevelFilters();

    rightLayout_->addWidget(toolBar_);
}

void TelemetryMdiWindow::setupLevelFilters() {

    // Create filter buttons with colors matching the log level badges
    auto createFilterBtn = [this](const QString& text, bool& state, const QColor& color) {
        auto* btn = new QPushButton(text, toolBar_);
        btn->setCheckable(true);
        btn->setChecked(state);
        btn->setFixedHeight(24);
        btn->setMinimumWidth(60);
        updateFilterButtonStyle(btn, state, color);

        connect(btn, &QPushButton::toggled, this, [this, btn, &state, color](bool checked) {
            state = checked;
            updateFilterButtonStyle(btn, checked, color);
            applyLevelFilter();
        });

        return btn;
    };

    filterTraceBtn_ = createFilterBtn("TRACE", showTrace_, color_constants::level_trace);
    filterDebugBtn_ = createFilterBtn("DEBUG", showDebug_, color_constants::level_debug);
    filterInfoBtn_ = createFilterBtn("INFO", showInfo_, color_constants::level_info);
    filterWarnBtn_ = createFilterBtn("WARN", showWarn_, color_constants::level_warn);
    filterErrorBtn_ = createFilterBtn("ERROR", showError_, color_constants::level_error);

    toolBar_->addWidget(filterTraceBtn_);
    toolBar_->addWidget(filterDebugBtn_);
    toolBar_->addWidget(filterInfoBtn_);
    toolBar_->addWidget(filterWarnBtn_);
    toolBar_->addWidget(filterErrorBtn_);
}

void TelemetryMdiWindow::updateFilterButtonStyle(QPushButton* btn, bool enabled, const QColor& color) {
    if (enabled) {
        btn->setStyleSheet(QString(
            "QPushButton { background-color: %1; color: white; border: none; "
            "border-radius: 3px; padding: 2px 8px; font-weight: bold; font-size: 10px; }"
            "QPushButton:hover { background-color: %2; }")
            .arg(color.name())
            .arg(color.lighter(110).name()));
    } else {
        btn->setStyleSheet(
            "QPushButton { background-color: #555; color: #888; border: none; "
            "border-radius: 3px; padding: 2px 8px; font-weight: bold; font-size: 10px; }"
            "QPushButton:hover { background-color: #666; }");
    }
}

void TelemetryMdiWindow::applyLevelFilter() {
    // Build regex pattern for enabled levels
    QStringList enabledLevels;
    if (showTrace_) enabledLevels << "trace";
    if (showDebug_) enabledLevels << "debug";
    if (showInfo_) enabledLevels << "info";
    if (showWarn_) enabledLevels << "warn" << "warning";
    if (showError_) enabledLevels << "error";

    if (enabledLevels.isEmpty()) {
        // No levels selected - show nothing
        proxyModel_->setFilterRegularExpression("^$");
    } else {
        // Filter on Level column (column 1)
        QString pattern = "^(" + enabledLevels.join("|") + ")$";
        proxyModel_->setFilterRegularExpression(QRegularExpression(pattern, QRegularExpression::CaseInsensitiveOption));
    }
    proxyModel_->setFilterKeyColumn(ClientTelemetryLogModel::Level);
}

void TelemetryMdiWindow::setupReloadAction() {
    normalReloadIcon_ = IconUtils::createRecoloredIcon(
        Icon::ArrowSync, IconUtils::DefaultIconColor);
    staleReloadIcon_ = IconUtils::createRecoloredIcon(
        Icon::ArrowSync, color_constants::stale_indicator);

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
    sessionTree_ = new QTreeWidget(this);
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

    // Set column widths (Tag before Message, Message stretches)
    header->resizeSection(ClientTelemetryLogModel::Timestamp, 180);
    header->resizeSection(ClientTelemetryLogModel::Level, 70);
    header->resizeSection(ClientTelemetryLogModel::Source, 120);
    header->resizeSection(ClientTelemetryLogModel::Component, 180);
    header->resizeSection(ClientTelemetryLogModel::Tag, 80);

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
    // Log detail panel - just the message text
    logDetailGroup_ = new QGroupBox(tr("Message"), rightPanel_);
    auto* logLayout = new QVBoxLayout(logDetailGroup_);
    logLayout->setContentsMargins(5, 5, 5, 5);

    logMessageEdit_ = new QTextEdit(logDetailGroup_);
    logMessageEdit_->setReadOnly(true);
    logMessageEdit_->setMaximumHeight(80);
    logLayout->addWidget(logMessageEdit_);

    rightLayout_->addWidget(logDetailGroup_);

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

        // Use username if available, otherwise use "(unknown)"
        QString userKey = QString::fromStdString(session.username);
        if (userKey.isEmpty()) {
            userKey = tr("(unknown)");
        }

        std::string idStr = boost::uuids::to_string(session.id);
        sessionCache_[idStr] = session;
        grouped[dateKey][userKey].push_back(&sessionCache_[idStr]);
    }

    // Build tree structure (newest dates first)
    for (auto dateIt = grouped.rbegin(); dateIt != grouped.rend(); ++dateIt) {
        const QString& date = dateIt->first;
        auto* dateItem = new QTreeWidgetItem(sessionTree_);
        dateItem->setText(0, date);
        dateItem->setExpanded(true);

        for (const auto& [username, userSessions] : dateIt->second) {
            auto* userItem = new QTreeWidgetItem(dateItem);
            userItem->setText(0, QString("%1 (%2)")
                .arg(username).arg(userSessions.size()));
            userItem->setExpanded(true);

            for (const auto* session : userSessions) {
                auto* sessionItem = new QTreeWidgetItem(userItem);
                std::string idStr = boost::uuids::to_string(session->id);
                sessionItem->setText(0, QString::fromStdString(idStr));
                sessionItem->setData(0, Qt::UserRole, QString::fromStdString(idStr));

                // Add visual indicator for active sessions
                if (session->is_active()) {
                    sessionItem->setForeground(0, QBrush(color_constants::active_session));
                }
            }
        }
    }

    emit statusChanged(tr("Loaded %1 sessions").arg(sessions.size()));
}

void TelemetryMdiWindow::onSessionSelected(QTreeWidgetItem* item, int column) {
    Q_UNUSED(column);

    QString sessionId = item->data(0, Qt::UserRole).toString();
    if (sessionId.isEmpty()) {
        return;
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
    sessionDetailsTable_->setRowCount(0);

    auto addRow = [this](const QString& field, const QString& value) {
        int row = sessionDetailsTable_->rowCount();
        sessionDetailsTable_->insertRow(row);
        sessionDetailsTable_->setItem(row, 0, new QTableWidgetItem(field));
        sessionDetailsTable_->setItem(row, 1, new QTableWidgetItem(value));
    };

    // Session ID
    addRow(tr("Session ID"), QString::fromStdString(
        boost::uuids::to_string(session.id)));

    // Account ID
    addRow(tr("Account ID"), QString::fromStdString(
        boost::uuids::to_string(session.account_id)));

    // Username
    QString username = QString::fromStdString(session.username);
    addRow(tr("Username"), username.isEmpty() ? tr("(not set)") : username);

    // Client
    addRow(tr("Client"), QString::fromStdString(session.client_identifier));

    // Version
    addRow(tr("Version"), QString("%1.%2")
        .arg(session.client_version_major)
        .arg(session.client_version_minor));

    // Start time
    const auto startMsecs = std::chrono::duration_cast<std::chrono::milliseconds>(
        session.start_time.time_since_epoch()).count();
    QDateTime start = QDateTime::fromMSecsSinceEpoch(startMsecs);
    addRow(tr("Start Time"), start.toString("yyyy-MM-dd hh:mm:ss"));

    // End time / Status
    if (session.is_active()) {
        addRow(tr("Status"), tr("Active"));
    } else {
        const auto endMsecs = std::chrono::duration_cast<std::chrono::milliseconds>(
            session.end_time->time_since_epoch()).count();
        QDateTime end = QDateTime::fromMSecsSinceEpoch(endMsecs);
        addRow(tr("End Time"), end.toString("yyyy-MM-dd hh:mm:ss"));

        auto duration = session.duration();
        if (duration) {
            int mins = static_cast<int>(duration->count() / 60);
            int secs = static_cast<int>(duration->count() % 60);
            addRow(tr("Duration"), QString("%1m %2s").arg(mins).arg(secs));
        }
    }

    // IP Address
    addRow(tr("IP Address"), QString::fromStdString(session.client_ip.to_string()));

    // Country code
    if (!session.country_code.empty()) {
        addRow(tr("Country"), QString::fromStdString(session.country_code));
    }

    // Protocol
    addRow(tr("Protocol"), QString::fromStdString(
        std::string(iam::domain::to_string(session.protocol))));

    // Bytes transferred
    addRow(tr("Bytes Sent"), QString::number(session.bytes_sent));
    addRow(tr("Bytes Received"), QString::number(session.bytes_received));
}

void TelemetryMdiWindow::
updateLogDetailPanel(const telemetry::domain::telemetry_log_entry& entry) {
    logMessageEdit_->setText(QString::fromStdString(entry.message));
}

void TelemetryMdiWindow::clearDetailPanels() {
    sessionDetailsTable_->setRowCount(0);
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

void TelemetryMdiWindow::onLoadError(const QString& error_message,
                                      const QString& details) {
    BOOST_LOG_SEV(lg(), error) << "Load error: " << error_message.toStdString();
    emit errorOccurred(error_message);
    MessageBoxHelper::critical(this, tr("Load Error"), error_message, details);
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
