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
#ifndef ORES_QT_SESSION_HISTORY_DIALOG_HPP
#define ORES_QT_SESSION_HISTORY_DIALOG_HPP

#include <QDialog>
#include <QTableView>
#include <QVBoxLayout>
#include <QAbstractTableModel>
#include <QFutureWatcher>
#include <memory>
#include <boost/uuid/uuid.hpp>
#include "ores.qt/ClientManager.hpp"
#include "ores.telemetry/log/make_logger.hpp"
#include "ores.iam/domain/session.hpp"

namespace ores::qt {

/**
 * @brief Table model for displaying session history.
 */
class SessionHistoryModel : public QAbstractTableModel {
    Q_OBJECT

public:
    explicit SessionHistoryModel(QObject* parent = nullptr);

    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    int columnCount(const QModelIndex& parent = QModelIndex()) const override;
    QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const override;
    QVariant headerData(int section, Qt::Orientation orientation,
        int role = Qt::DisplayRole) const override;

    void setSessions(const std::vector<iam::domain::session>& sessions);
    void clear();

private:
    enum Column {
        StartTime,
        EndTime,
        Duration,
        ClientIP,
        Country,
        City,
        BytesSent,
        BytesReceived,
        ClientVersion,
        ColumnCount
    };

    std::vector<iam::domain::session> sessions_;
};

/**
 * @brief Dialog for displaying session history for an account.
 *
 * Shows a table of all sessions with start/end times, durations,
 * bytes transferred, and geolocation information.
 */
class SessionHistoryDialog : public QDialog {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.session_history_dialog";

    [[nodiscard]] static auto& lg() {
        using namespace ores::telemetry::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit SessionHistoryDialog(ClientManager* clientManager,
                                  QWidget* parent = nullptr);
    ~SessionHistoryDialog() override;

    /**
     * @brief Set the account to display sessions for.
     *
     * @param accountId The account UUID
     * @param username The account username (for display)
     */
    void setAccount(const boost::uuids::uuid& accountId,
                    const QString& username);

    /**
     * @brief Refresh the session list from the server.
     */
    void refresh();

signals:
    void statusMessage(const QString& message);
    void errorMessage(const QString& message);

private slots:
    void onSessionsLoaded();

private:
    void setupUi();

    QTableView* tableView_;
    SessionHistoryModel* model_;
    ClientManager* clientManager_;
    boost::uuids::uuid accountId_;
    QString username_;

    struct FetchResult {
        bool success;
        std::vector<iam::domain::session> sessions;
        std::uint32_t total_count;
    };

    QFutureWatcher<FetchResult>* watcher_;
};

}

#endif
