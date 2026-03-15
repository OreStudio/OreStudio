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
#ifndef ORES_QT_QUEUE_DETAIL_DIALOG_HPP
#define ORES_QT_QUEUE_DETAIL_DIALOG_HPP

#include <cstdint>
#include <vector>
#include <QWidget>
#include <QToolBar>
#include <QTabWidget>
#include <QSpinBox>
#include <QComboBox>
#include <QLineEdit>
#include <QTableWidget>
#include <QPlainTextEdit>
#include <QLabel>
#include <QFutureWatcher>
#include "ores.qt/ClientManager.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::qt {

/**
 * @brief Detail widget for a single JetStream stream.
 *
 * Two-tab interface:
 *  - Publish: compose and send a message to a JetStream subject.
 *  - Messages: peek messages by sequence range, view and delete individual ones.
 */
class QueueDetailDialog final : public QWidget {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.queue_detail_dialog";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit QueueDetailDialog(const QString& streamName,
                               const QString& displayName,
                               ClientManager* clientManager,
                               QWidget* parent = nullptr);
    ~QueueDetailDialog() override = default;

    QSize sizeHint() const override { return {750, 520}; }

signals:
    void statusChanged(const QString& message);
    void errorOccurred(const QString& error_message);

private slots:
    void onPublish();
    void onGetMessages();
    void onDeleteSelected();
    void onPublishDone();
    void onGetMessagesDone();

private:
    enum class GetMode { Peek, Pop };

    struct PublishResult {
        bool success{false};
        QString error_message;
        QString error_details;
    };

    struct MessageRow {
        std::uint64_t sequence{0};
        std::string subject;
        std::string timestamp;   // formatted string
        std::string payload;
    };

    struct GetMessagesResult {
        bool success{false};
        std::vector<MessageRow> messages;
        GetMode mode{GetMode::Peek};
        QString error_message;
        QString error_details;
    };

    void setupUi();
    void setupToolbar();
    void setupPublishTab();
    void setupMessagesTab();
    void populateMessagesTable(const std::vector<MessageRow>& messages);
    void updateDeleteAction();

    QString streamName_;    // NATS stream name (used as identifier)
    QString displayName_;   // human-readable name shown in UI
    ClientManager* clientManager_;

    QToolBar* toolbar_;
    QAction* publishAction_;
    QAction* getMessagesAction_;
    QAction* deleteAction_;
    QTabWidget* tabWidget_;

    // Publish tab
    QWidget* publishTab_;
    QLineEdit* subjectEdit_;
    QPlainTextEdit* payloadEdit_;

    // Messages tab
    QWidget* messagesTab_;
    QComboBox* modeCombo_;
    QSpinBox* startSeqSpinBox_;
    QSpinBox* countSpinBox_;
    QTableWidget* messagesTable_;

    QFutureWatcher<PublishResult>* publishWatcher_;
    QFutureWatcher<GetMessagesResult>* getWatcher_;
};

}

#endif
