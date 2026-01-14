/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2024-2025 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301, USA.
 *
 */
#include "ores.qt/EventViewerDialog.hpp"
#include "ores.qt/ClientManager.hpp"

#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QTableView>
#include <QHeaderView>
#include <QLabel>
#include <QPushButton>
#include <QMessageBox>
#include <QCloseEvent>
#include <QJsonDocument>
#include <QJsonObject>
#include <QJsonArray>
#include <QApplication>
#include <QClipboard>
#include <QMenu>
#include <QDialog>
#include <QTextEdit>
#include <QDialogButtonBox>

// Event types to subscribe to
#include "ores.comms/eventing/connection_events.hpp"
#include "ores.refdata/eventing/currency_changed_event.hpp"
#include "ores.variability/eventing/feature_flags_changed_event.hpp"
#include "ores.iam/eventing/account_changed_event.hpp"
#include "ores.iam/eventing/role_assigned_event.hpp"
#include "ores.iam/eventing/role_revoked_event.hpp"
#include "ores.iam/eventing/account_permissions_changed_event.hpp"
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

using namespace ores::logging;

namespace {

QString formatTimestamp(std::chrono::system_clock::time_point tp) {
    auto time_t = std::chrono::system_clock::to_time_t(tp);
    auto ms = std::chrono::duration_cast<std::chrono::milliseconds>(
        tp.time_since_epoch()) % 1000;
    std::tm tm{};
#ifdef _WIN32
    localtime_s(&tm, &time_t);
#else
    localtime_r(&time_t, &tm);
#endif
    char buffer[32];
    std::strftime(buffer, sizeof(buffer), "%Y-%m-%d %H:%M:%S", &tm);
    return QString("%1.%2").arg(buffer).arg(ms.count(), 3, 10, QChar('0'));
}

QDateTime toQDateTime(std::chrono::system_clock::time_point tp) {
    auto ms = std::chrono::duration_cast<std::chrono::milliseconds>(
        tp.time_since_epoch()).count();
    return QDateTime::fromMSecsSinceEpoch(ms);
}

}

// ============================================================================
// EventTableModel
// ============================================================================

EventTableModel::EventTableModel(QObject* parent)
    : QAbstractTableModel(parent) {
}

int EventTableModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return static_cast<int>(events_.size());
}

int EventTableModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return ColumnCount;
}

QVariant EventTableModel::data(const QModelIndex& index, int role) const {
    if (!index.isValid() || index.row() >= static_cast<int>(events_.size()))
        return {};

    const auto& event = events_[index.row()];

    if (role == Qt::DisplayRole) {
        switch (index.column()) {
        case Timestamp:
            return event.timestamp.toString("yyyy-MM-dd hh:mm:ss.zzz");
        case EventType:
            return event.eventType;
        case Source:
            return event.source;
        case Summary:
            return event.summary;
        }
    }

    return {};
}

QVariant EventTableModel::headerData(int section, Qt::Orientation orientation,
                                      int role) const {
    if (orientation != Qt::Horizontal || role != Qt::DisplayRole)
        return {};

    switch (section) {
    case Timestamp:
        return tr("Timestamp");
    case EventType:
        return tr("Event Type");
    case Source:
        return tr("Source");
    case Summary:
        return tr("Summary");
    }

    return {};
}

void EventTableModel::addEvent(EventRecord event) {
    // Remove oldest events if we exceed max
    while (events_.size() >= max_events_) {
        beginRemoveRows(QModelIndex(), 0, 0);
        events_.pop_front();
        endRemoveRows();
    }

    // Add new event at the end (newest at bottom)
    const int row = static_cast<int>(events_.size());
    beginInsertRows(QModelIndex(), row, row);
    events_.push_back(std::move(event));
    endInsertRows();
}

void EventTableModel::clear() {
    if (events_.empty())
        return;

    beginResetModel();
    events_.clear();
    endResetModel();
}

const EventRecord& EventTableModel::eventAt(int row) const {
    return events_.at(row);
}

// ============================================================================
// EventViewerWindow
// ============================================================================

EventViewerWindow::EventViewerWindow(
    std::shared_ptr<eventing::service::event_bus> eventBus,
    ClientManager* clientManager,
    QWidget* parent)
    : QWidget(parent)
    , eventBus_(std::move(eventBus))
    , clientManager_(clientManager)
    , tableView_(nullptr)
    , model_(nullptr)
    , statusLabel_(nullptr)
    , clearButton_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "Creating event viewer window.";
    setupUi();
}

EventViewerWindow::~EventViewerWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Destroying event viewer window.";
    unsubscribeFromEvents();
}

void EventViewerWindow::setupUi() {
    setMinimumSize(800, 500);

    auto* layout = new QVBoxLayout(this);

    // Table view
    model_ = new EventTableModel(this);
    tableView_ = new QTableView(this);
    tableView_->setModel(model_);
    tableView_->setSelectionBehavior(QAbstractItemView::SelectRows);
    tableView_->setSelectionMode(QAbstractItemView::SingleSelection);
    tableView_->setAlternatingRowColors(true);
    tableView_->setSortingEnabled(false);
    tableView_->verticalHeader()->setVisible(false);
    tableView_->horizontalHeader()->setStretchLastSection(true);
    tableView_->setContextMenuPolicy(Qt::CustomContextMenu);

    // Set column widths - ensure timestamp and event type are fully visible
    tableView_->setColumnWidth(EventTableModel::Timestamp, 200);
    tableView_->setColumnWidth(EventTableModel::EventType, 280);
    tableView_->setColumnWidth(EventTableModel::Source, 70);

    layout->addWidget(tableView_);

    // Bottom row: status and clear button
    auto* bottomLayout = new QHBoxLayout();

    statusLabel_ = new QLabel(tr("Events: 0"), this);
    bottomLayout->addWidget(statusLabel_);

    bottomLayout->addStretch();

    clearButton_ = new QPushButton(tr("Clear"), this);
    bottomLayout->addWidget(clearButton_);

    layout->addLayout(bottomLayout);

    // Connections
    connect(tableView_, &QTableView::doubleClicked,
            this, &EventViewerWindow::onEventDoubleClicked);
    connect(clearButton_, &QPushButton::clicked,
            this, &EventViewerWindow::onClearClicked);
}

void EventViewerWindow::showEvent(QShowEvent* event) {
    QWidget::showEvent(event);
    BOOST_LOG_SEV(lg(), info) << "Event viewer shown - subscribing to events.";
    subscribeToEvents();
}

void EventViewerWindow::closeEvent(QCloseEvent* event) {
    BOOST_LOG_SEV(lg(), info) << "Event viewer closing - unsubscribing from events.";
    unsubscribeFromEvents();
    QWidget::closeEvent(event);
}

void EventViewerWindow::subscribeToEvents() {
    if (!eventBus_) {
        BOOST_LOG_SEV(lg(), warn) << "No event bus available.";
        return;
    }

    // Subscribe to connection events
    subscriptions_.push_back(
        eventBus_->subscribe<comms::eventing::connected_event>(
            [this](const comms::eventing::connected_event& e) {
                QJsonObject json;
                json["host"] = QString::fromStdString(e.host);
                json["port"] = e.port;
                json["timestamp"] = formatTimestamp(e.timestamp);

                EventRecord record{
                    toQDateTime(e.timestamp),
                    QString::fromUtf8(eventing::domain::event_traits<
                        comms::eventing::connected_event>::name),
                    "local",
                    QString("Connected to %1:%2")
                        .arg(QString::fromStdString(e.host))
                        .arg(e.port),
                    QString::fromUtf8(
                        QJsonDocument(json).toJson(QJsonDocument::Indented))
                };

                QMetaObject::invokeMethod(this, [this, r = std::move(record)]() {
                    addEvent(std::move(r));
                }, Qt::QueuedConnection);
            }));

    subscriptions_.push_back(
        eventBus_->subscribe<comms::eventing::disconnected_event>(
            [this](const comms::eventing::disconnected_event& e) {
                QJsonObject json;
                json["timestamp"] = formatTimestamp(e.timestamp);

                EventRecord record{
                    toQDateTime(e.timestamp),
                    QString::fromUtf8(eventing::domain::event_traits<
                        comms::eventing::disconnected_event>::name),
                    "local",
                    "Disconnected from server",
                    QString::fromUtf8(
                        QJsonDocument(json).toJson(QJsonDocument::Indented))
                };

                QMetaObject::invokeMethod(this, [this, r = std::move(record)]() {
                    addEvent(std::move(r));
                }, Qt::QueuedConnection);
            }));

    subscriptions_.push_back(
        eventBus_->subscribe<comms::eventing::reconnecting_event>(
            [this](const comms::eventing::reconnecting_event& e) {
                QJsonObject json;
                json["timestamp"] = formatTimestamp(e.timestamp);

                EventRecord record{
                    toQDateTime(e.timestamp),
                    QString::fromUtf8(eventing::domain::event_traits<
                        comms::eventing::reconnecting_event>::name),
                    "local",
                    "Attempting to reconnect...",
                    QString::fromUtf8(
                        QJsonDocument(json).toJson(QJsonDocument::Indented))
                };

                QMetaObject::invokeMethod(this, [this, r = std::move(record)]() {
                    addEvent(std::move(r));
                }, Qt::QueuedConnection);
            }));

    subscriptions_.push_back(
        eventBus_->subscribe<comms::eventing::reconnected_event>(
            [this](const comms::eventing::reconnected_event& e) {
                QJsonObject json;
                json["timestamp"] = formatTimestamp(e.timestamp);

                EventRecord record{
                    toQDateTime(e.timestamp),
                    QString::fromUtf8(eventing::domain::event_traits<
                        comms::eventing::reconnected_event>::name),
                    "local",
                    "Reconnected to server",
                    QString::fromUtf8(
                        QJsonDocument(json).toJson(QJsonDocument::Indented))
                };

                QMetaObject::invokeMethod(this, [this, r = std::move(record)]() {
                    addEvent(std::move(r));
                }, Qt::QueuedConnection);
            }));

    // Subscribe to currency changed events
    subscriptions_.push_back(
        eventBus_->subscribe<refdata::eventing::currency_changed_event>(
            [this](const refdata::eventing::currency_changed_event& e) {
                QJsonObject json;
                json["timestamp"] = formatTimestamp(e.timestamp);
                QJsonArray codes;
                for (const auto& code : e.iso_codes) {
                    codes.append(QString::fromStdString(code));
                }
                json["iso_codes"] = codes;

                EventRecord record{
                    toQDateTime(e.timestamp),
                    QString::fromUtf8(eventing::domain::event_traits<
                        refdata::eventing::currency_changed_event>::name),
                    "local",
                    QString("%1 currency(ies) changed").arg(e.iso_codes.size()),
                    QString::fromUtf8(
                        QJsonDocument(json).toJson(QJsonDocument::Indented))
                };

                QMetaObject::invokeMethod(this, [this, r = std::move(record)]() {
                    addEvent(std::move(r));
                }, Qt::QueuedConnection);
            }));

    // Subscribe to feature flags changed events
    subscriptions_.push_back(
        eventBus_->subscribe<variability::eventing::feature_flags_changed_event>(
            [this](const variability::eventing::feature_flags_changed_event& e) {
                QJsonObject json;
                json["timestamp"] = formatTimestamp(e.timestamp);

                EventRecord record{
                    toQDateTime(e.timestamp),
                    QString::fromUtf8(eventing::domain::event_traits<
                        variability::eventing::feature_flags_changed_event>::name),
                    "local",
                    "Feature flags changed",
                    QString::fromUtf8(
                        QJsonDocument(json).toJson(QJsonDocument::Indented))
                };

                QMetaObject::invokeMethod(this, [this, r = std::move(record)]() {
                    addEvent(std::move(r));
                }, Qt::QueuedConnection);
            }));

    // Subscribe to IAM events
    subscriptions_.push_back(
        eventBus_->subscribe<iam::eventing::account_changed_event>(
            [this](const iam::eventing::account_changed_event& e) {
                QJsonObject json;
                json["timestamp"] = formatTimestamp(e.timestamp);
                QJsonArray ids;
                for (const auto& id : e.account_ids) {
                    ids.append(QString::fromStdString(id));
                }
                json["account_ids"] = ids;

                EventRecord record{
                    toQDateTime(e.timestamp),
                    QString::fromUtf8(eventing::domain::event_traits<
                        iam::eventing::account_changed_event>::name),
                    "local",
                    QString("%1 account(s) changed").arg(e.account_ids.size()),
                    QString::fromUtf8(
                        QJsonDocument(json).toJson(QJsonDocument::Indented))
                };

                QMetaObject::invokeMethod(this, [this, r = std::move(record)]() {
                    addEvent(std::move(r));
                }, Qt::QueuedConnection);
            }));

    subscriptions_.push_back(
        eventBus_->subscribe<iam::eventing::role_assigned_event>(
            [this](const iam::eventing::role_assigned_event& e) {
                QJsonObject json;
                json["timestamp"] = formatTimestamp(e.timestamp);
                json["account_id"] = QString::fromStdString(
                    boost::uuids::to_string(e.account_id));
                json["role_id"] = QString::fromStdString(
                    boost::uuids::to_string(e.role_id));

                EventRecord record{
                    toQDateTime(e.timestamp),
                    QString::fromUtf8(eventing::domain::event_traits<
                        iam::eventing::role_assigned_event>::name),
                    "local",
                    "Role assigned",
                    QString::fromUtf8(
                        QJsonDocument(json).toJson(QJsonDocument::Indented))
                };

                QMetaObject::invokeMethod(this, [this, r = std::move(record)]() {
                    addEvent(std::move(r));
                }, Qt::QueuedConnection);
            }));

    subscriptions_.push_back(
        eventBus_->subscribe<iam::eventing::role_revoked_event>(
            [this](const iam::eventing::role_revoked_event& e) {
                QJsonObject json;
                json["timestamp"] = formatTimestamp(e.timestamp);
                json["account_id"] = QString::fromStdString(
                    boost::uuids::to_string(e.account_id));
                json["role_id"] = QString::fromStdString(
                    boost::uuids::to_string(e.role_id));

                EventRecord record{
                    toQDateTime(e.timestamp),
                    QString::fromUtf8(eventing::domain::event_traits<
                        iam::eventing::role_revoked_event>::name),
                    "local",
                    "Role revoked",
                    QString::fromUtf8(
                        QJsonDocument(json).toJson(QJsonDocument::Indented))
                };

                QMetaObject::invokeMethod(this, [this, r = std::move(record)]() {
                    addEvent(std::move(r));
                }, Qt::QueuedConnection);
            }));

    subscriptions_.push_back(
        eventBus_->subscribe<iam::eventing::account_permissions_changed_event>(
            [this](const iam::eventing::account_permissions_changed_event& e) {
                QJsonObject json;
                json["timestamp"] = formatTimestamp(e.timestamp);
                json["account_id"] = QString::fromStdString(
                    boost::uuids::to_string(e.account_id));
                QJsonArray perms;
                for (const auto& perm : e.permission_codes) {
                    perms.append(QString::fromStdString(perm));
                }
                json["permission_codes"] = perms;

                EventRecord record{
                    toQDateTime(e.timestamp),
                    QString::fromUtf8(eventing::domain::event_traits<
                        iam::eventing::account_permissions_changed_event>::name),
                    "local",
                    QString("Permissions changed (%1 permission(s))")
                        .arg(e.permission_codes.size()),
                    QString::fromUtf8(
                        QJsonDocument(json).toJson(QJsonDocument::Indented))
                };

                QMetaObject::invokeMethod(this, [this, r = std::move(record)]() {
                    addEvent(std::move(r));
                }, Qt::QueuedConnection);
            }));

    // Connect to ClientManager for remote notifications
    if (clientManager_ && !connectedToClientManager_) {
        connect(clientManager_, &ClientManager::notificationReceived,
                this, &EventViewerWindow::onNotificationReceived);
        connectedToClientManager_ = true;
        BOOST_LOG_SEV(lg(), debug) << "Connected to ClientManager notifications.";
    }

    BOOST_LOG_SEV(lg(), info) << "Subscribed to " << subscriptions_.size()
                              << " local event type(s).";
}

void EventViewerWindow::unsubscribeFromEvents() {
    // Clear subscriptions - RAII handles unsubscription
    const auto count = subscriptions_.size();
    subscriptions_.clear();

    // Disconnect from ClientManager
    if (clientManager_ && connectedToClientManager_) {
        disconnect(clientManager_, &ClientManager::notificationReceived,
                   this, &EventViewerWindow::onNotificationReceived);
        connectedToClientManager_ = false;
    }

    BOOST_LOG_SEV(lg(), info) << "Unsubscribed from " << count
                              << " event type(s).";
}

void EventViewerWindow::addEvent(EventRecord record) {
    model_->addEvent(std::move(record));
    statusLabel_->setText(tr("Events: %1").arg(model_->eventCount()));

    // Auto-scroll to bottom
    tableView_->scrollToBottom();
}

void EventViewerWindow::onEventDoubleClicked(const QModelIndex& index) {
    if (!index.isValid())
        return;

    const auto& event = model_->eventAt(index.row());

    // Create a detail dialog
    QDialog detailDialog(this);
    detailDialog.setWindowTitle(tr("Event Details - %1").arg(event.eventType));
    detailDialog.setMinimumSize(500, 400);
    detailDialog.resize(600, 500);

    auto* layout = new QVBoxLayout(&detailDialog);

    // Event info
    auto* infoLabel = new QLabel(
        tr("<b>Event Type:</b> %1<br>"
           "<b>Source:</b> %2<br>"
           "<b>Timestamp:</b> %3<br>"
           "<b>Summary:</b> %4")
            .arg(event.eventType)
            .arg(event.source)
            .arg(event.timestamp.toString("yyyy-MM-dd hh:mm:ss.zzz"))
            .arg(event.summary),
        &detailDialog);
    infoLabel->setTextFormat(Qt::RichText);
    layout->addWidget(infoLabel);

    // JSON payload
    auto* jsonLabel = new QLabel(tr("<b>JSON Payload:</b>"), &detailDialog);
    layout->addWidget(jsonLabel);

    auto* jsonText = new QTextEdit(&detailDialog);
    jsonText->setReadOnly(true);
    jsonText->setPlainText(event.jsonPayload);
    jsonText->setFont(QFont("Monospace", 10));
    layout->addWidget(jsonText);

    // Buttons
    auto* buttonBox = new QDialogButtonBox(QDialogButtonBox::Close, &detailDialog);
    auto* copyButton = buttonBox->addButton(tr("Copy JSON"), QDialogButtonBox::ActionRole);

    connect(copyButton, &QPushButton::clicked, this, [&event]() {
        QApplication::clipboard()->setText(event.jsonPayload);
    });
    connect(buttonBox, &QDialogButtonBox::rejected,
            &detailDialog, &QDialog::reject);

    layout->addWidget(buttonBox);

    detailDialog.exec();
}

void EventViewerWindow::onClearClicked() {
    model_->clear();
    statusLabel_->setText(tr("Events: 0"));
}

void EventViewerWindow::onNotificationReceived(const QString& eventType,
                                                const QDateTime& timestamp,
                                                const QStringList& entityIds) {
    QJsonObject json;
    json["event_type"] = eventType;
    json["timestamp"] = timestamp.toString(Qt::ISODateWithMs);
    QJsonArray ids;
    for (const auto& id : entityIds) {
        ids.append(id);
    }
    json["entity_ids"] = ids;

    QString summary;
    if (entityIds.isEmpty()) {
        summary = eventType;
    } else if (entityIds.size() == 1) {
        summary = QString("%1: %2").arg(eventType).arg(entityIds.first());
    } else {
        summary = QString("%1: %2 entities").arg(eventType).arg(entityIds.size());
    }

    EventRecord record{
        timestamp,
        eventType,
        "remote",
        summary,
        QString::fromUtf8(QJsonDocument(json).toJson(QJsonDocument::Indented))
    };

    QMetaObject::invokeMethod(this, [this, r = std::move(record)]() {
        addEvent(std::move(r));
    }, Qt::QueuedConnection);
}

}
