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
#ifndef ORES_QT_EVENT_VIEWER_DIALOG_HPP
#define ORES_QT_EVENT_VIEWER_DIALOG_HPP

#include <vector>
#include <memory>
#include <QDialog>
#include <QDateTime>
#include <QAbstractTableModel>
#include "ores.eventing/service/event_bus.hpp"
#include "ores.telemetry/log/make_logger.hpp"

class QTableView;
class QVBoxLayout;
class QLabel;
class QPushButton;

namespace ores::qt {

class ClientManager;

/**
 * @brief Record of a single event for display in the viewer.
 */
struct EventRecord {
    QDateTime timestamp;
    QString eventType;
    QString source;      // "local" or "remote"
    QString summary;
    QString jsonPayload;
};

/**
 * @brief Table model for displaying event records.
 */
class EventTableModel final : public QAbstractTableModel {
    Q_OBJECT

public:
    enum Column {
        Timestamp = 0,
        EventType,
        Source,
        Summary,
        ColumnCount
    };

    explicit EventTableModel(QObject* parent = nullptr);

    [[nodiscard]] int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    [[nodiscard]] int columnCount(const QModelIndex& parent = QModelIndex()) const override;
    [[nodiscard]] QVariant data(const QModelIndex& index,
                                int role = Qt::DisplayRole) const override;
    [[nodiscard]] QVariant headerData(int section, Qt::Orientation orientation,
                                       int role = Qt::DisplayRole) const override;

    void addEvent(EventRecord event);
    void clear();
    [[nodiscard]] const EventRecord& eventAt(int row) const;
    [[nodiscard]] int eventCount() const { return static_cast<int>(events_.size()); }

private:
    std::vector<EventRecord> events_;
    static constexpr int max_events_ = 1000;
};

/**
 * @brief Non-modal dialog for viewing domain events in real-time.
 *
 * When opened, the dialog subscribes to all known event types on the event bus
 * and also listens for remote notifications from the server via ClientManager.
 * When closed, all subscriptions are automatically cleaned up via RAII.
 *
 * Features:
 * - Real-time event display in a table view
 * - Columns: Timestamp, Event Type, Source, Summary
 * - Double-click an event to see its full JSON payload
 * - Zero cost when closed (no subscriptions active)
 */
class EventViewerDialog final : public QDialog {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.event_viewer_dialog";

    [[nodiscard]] static auto& lg() {
        using namespace ores::telemetry::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Construct the event viewer dialog.
     *
     * @param eventBus Shared pointer to the application event bus
     * @param clientManager Pointer to the client manager for remote events
     * @param parent Parent widget
     */
    explicit EventViewerDialog(
        std::shared_ptr<eventing::service::event_bus> eventBus,
        ClientManager* clientManager,
        QWidget* parent = nullptr);

    ~EventViewerDialog() override;

protected:
    void showEvent(QShowEvent* event) override;
    void closeEvent(QCloseEvent* event) override;

private slots:
    void onEventDoubleClicked(const QModelIndex& index);
    void onClearClicked();
    void onNotificationReceived(const QString& eventType,
                                const QDateTime& timestamp,
                                const QStringList& entityIds);

private:
    void setupUi();
    void subscribeToEvents();
    void unsubscribeFromEvents();

    /**
     * @brief Add an event to the model (thread-safe via Qt signal).
     */
    void addEvent(EventRecord record);

    std::shared_ptr<eventing::service::event_bus> eventBus_;
    ClientManager* clientManager_;

    // UI components
    QTableView* tableView_;
    EventTableModel* model_;
    QLabel* statusLabel_;
    QPushButton* clearButton_;

    // Event subscriptions (cleared on close)
    std::vector<eventing::service::subscription> subscriptions_;

    // Track if we're connected to ClientManager signals
    bool connectedToClientManager_{false};
};

}

#endif
