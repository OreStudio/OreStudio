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
#include "ores.qt/QueueDetailDialog.hpp"

#include <QPointer>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QFormLayout>
#include <QHeaderView>
#include <QtConcurrent>
#include "ores.platform/time/datetime.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.qt/MessageBoxHelper.hpp"

namespace ores::qt {

using namespace ores::logging;

QueueDetailDialog::QueueDetailDialog(const QString& streamName,
                                     const QString& displayName,
                                     ClientManager* clientManager,
                                     QWidget* parent)
    : QWidget(parent),
      streamName_(streamName),
      displayName_(displayName),
      clientManager_(clientManager),
      toolbar_(nullptr),
      publishAction_(nullptr),
      getMessagesAction_(nullptr),
      deleteAction_(nullptr),
      tabWidget_(nullptr),
      publishTab_(nullptr),
      subjectEdit_(nullptr),
      payloadEdit_(nullptr),
      messagesTab_(nullptr),
      modeCombo_(nullptr),
      startSeqSpinBox_(nullptr),
      countSpinBox_(nullptr),
      messagesTable_(nullptr),
      publishWatcher_(new QFutureWatcher<PublishResult>(this)),
      getWatcher_(new QFutureWatcher<GetMessagesResult>(this)) {

    connect(publishWatcher_, &QFutureWatcher<PublishResult>::finished,
            this, &QueueDetailDialog::onPublishDone);
    connect(getWatcher_, &QFutureWatcher<GetMessagesResult>::finished,
            this, &QueueDetailDialog::onGetMessagesDone);

    setupUi();
}

void QueueDetailDialog::setupUi() {
    auto* layout = new QVBoxLayout(this);
    layout->setContentsMargins(0, 0, 0, 0);
    layout->setSpacing(0);

    setupToolbar();
    layout->addWidget(toolbar_);

    tabWidget_ = new QTabWidget(this);
    setupPublishTab();
    setupMessagesTab();
    tabWidget_->addTab(publishTab_,  tr("Publish"));
    tabWidget_->addTab(messagesTab_, tr("Messages"));

    connect(tabWidget_, &QTabWidget::currentChanged, this, [this](int idx) {
        publishAction_->setVisible(idx == 0);
        getMessagesAction_->setVisible(idx == 1);
        deleteAction_->setVisible(idx == 1);
    });

    layout->addWidget(tabWidget_);
}

void QueueDetailDialog::setupToolbar() {
    toolbar_ = new QToolBar(this);
    toolbar_->setMovable(false);
    toolbar_->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    toolbar_->setIconSize(QSize(20, 20));

    publishAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Publish, IconUtils::DefaultIconColor),
        tr("Publish"));
    publishAction_->setToolTip(tr("Send the payload to the stream subject"));
    connect(publishAction_, &QAction::triggered,
            this, &QueueDetailDialog::onPublish);

    getMessagesAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::DocumentTable, IconUtils::DefaultIconColor),
        tr("Get Messages"));
    getMessagesAction_->setToolTip(tr("Peek messages from the stream by sequence"));
    getMessagesAction_->setVisible(false);
    connect(getMessagesAction_, &QAction::triggered,
            this, &QueueDetailDialog::onGetMessages);

    deleteAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor),
        tr("Delete"));
    deleteAction_->setToolTip(tr("Delete selected messages from the stream"));
    deleteAction_->setEnabled(false);
    deleteAction_->setVisible(false);
    connect(deleteAction_, &QAction::triggered,
            this, &QueueDetailDialog::onDeleteSelected);
}

void QueueDetailDialog::setupPublishTab() {
    publishTab_ = new QWidget();
    auto* layout = new QVBoxLayout(publishTab_);
    layout->setContentsMargins(8, 8, 8, 8);

    auto* formLayout = new QFormLayout();
    subjectEdit_ = new QLineEdit();
    subjectEdit_->setPlaceholderText(tr("e.g. ores.dev.local1.iam.v1.accounts.created"));
    subjectEdit_->setToolTip(tr("NATS subject to publish to (must be covered by this stream)"));
    formLayout->addRow(tr("Subject:"), subjectEdit_);
    layout->addLayout(formLayout);

    auto* payloadLabel = new QLabel(tr("Payload (JSON):"));
    layout->addWidget(payloadLabel);

    payloadEdit_ = new QPlainTextEdit();
    payloadEdit_->setPlaceholderText(tr("{ }"));
    QFont mono = payloadEdit_->font();
    mono.setFamily("Monospace");
    payloadEdit_->setFont(mono);
    layout->addWidget(payloadEdit_, 1);
}

void QueueDetailDialog::setupMessagesTab() {
    messagesTab_ = new QWidget();
    auto* layout = new QVBoxLayout(messagesTab_);
    layout->setContentsMargins(8, 8, 8, 8);
    layout->setSpacing(6);

    // Controls row
    auto* controls = new QHBoxLayout();
    controls->addWidget(new QLabel(tr("Mode:")));
    modeCombo_ = new QComboBox();
    modeCombo_->addItem(tr("Peek (non-destructive)"), QVariant::fromValue(int(GetMode::Peek)));
    modeCombo_->addItem(tr("Pop (peek + delete)"),    QVariant::fromValue(int(GetMode::Pop)));
    controls->addWidget(modeCombo_);

    controls->addSpacing(16);
    controls->addWidget(new QLabel(tr("Start Seq:")));
    startSeqSpinBox_ = new QSpinBox();
    startSeqSpinBox_->setRange(1, 999999999);
    startSeqSpinBox_->setValue(1);
    startSeqSpinBox_->setToolTip(tr("First sequence number to read"));
    controls->addWidget(startSeqSpinBox_);

    controls->addSpacing(16);
    controls->addWidget(new QLabel(tr("Count:")));
    countSpinBox_ = new QSpinBox();
    countSpinBox_->setRange(1, 100);
    countSpinBox_->setValue(10);
    controls->addWidget(countSpinBox_);

    controls->addStretch();
    layout->addLayout(controls);

    auto* warning = new QLabel(
        tr("<small><i>Pop is destructive — messages cannot be recovered.</i></small>"));
    warning->setWordWrap(true);
    layout->addWidget(warning);

    // Results table: Seq | Subject | Timestamp | Payload
    messagesTable_ = new QTableWidget(0, 4, this);
    messagesTable_->setHorizontalHeaderLabels(
        {tr("Seq"), tr("Subject"), tr("Timestamp (UTC)"), tr("Payload")});
    messagesTable_->setSelectionBehavior(QAbstractItemView::SelectRows);
    messagesTable_->setSelectionMode(QAbstractItemView::ExtendedSelection);
    messagesTable_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    messagesTable_->setAlternatingRowColors(true);
    messagesTable_->verticalHeader()->setVisible(false);
    messagesTable_->horizontalHeader()->setStretchLastSection(true);
    messagesTable_->horizontalHeader()->setSectionResizeMode(
        QHeaderView::ResizeToContents);
    layout->addWidget(messagesTable_, 1);

    connect(messagesTable_, &QTableWidget::itemSelectionChanged,
            this, &QueueDetailDialog::updateDeleteAction);
}

void QueueDetailDialog::onPublish() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        emit errorOccurred(tr("Not connected to server"));
        return;
    }

    const QString subject = subjectEdit_->text().trimmed();
    if (subject.isEmpty()) {
        emit errorOccurred(tr("Subject is empty"));
        return;
    }

    const QString payload = payloadEdit_->toPlainText().trimmed();
    if (payload.isEmpty()) {
        emit errorOccurred(tr("Payload is empty"));
        return;
    }

    publishAction_->setEnabled(false);
    emit statusChanged(tr("Publishing to '%1'…").arg(subject));

    QPointer<QueueDetailDialog> self = this;

    QFuture<PublishResult> future = QtConcurrent::run(
        [self, subject, payload]() -> PublishResult {
            return exception_helper::wrap_async_fetch<PublishResult>(
                [&]() -> PublishResult {
                    if (!self || !self->clientManager_) {
                        return {.success = false,
                                .error_message = "Dialog closed", .error_details = {}};
                    }

                    auto admin = self->clientManager_->admin();
                    admin.publish(subject.toStdString(), payload.toStdString());
                    return {.success = true, .error_message = {}, .error_details = {}};
                }, "publish message");
        });

    publishWatcher_->setFuture(future);
}

void QueueDetailDialog::onPublishDone() {
    publishAction_->setEnabled(true);

    const auto result = publishWatcher_->result();
    if (result.success) {
        emit statusChanged(tr("Message published to '%1'").arg(subjectEdit_->text()));
    } else {
        BOOST_LOG_SEV(lg(), error) << "Publish failed: "
                                   << result.error_message.toStdString();
        emit errorOccurred(result.error_message);
        MessageBoxHelper::critical(this, tr("Publish Error"),
                                   result.error_message, result.error_details);
    }
}

void QueueDetailDialog::onGetMessages() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        emit errorOccurred(tr("Not connected to server"));
        return;
    }

    getMessagesAction_->setEnabled(false);
    messagesTable_->setRowCount(0);

    const GetMode mode = static_cast<GetMode>(modeCombo_->currentData().toInt());
    const QString streamName = streamName_;
    const auto startSeq = static_cast<std::uint64_t>(startSeqSpinBox_->value());
    const int count = countSpinBox_->value();

    emit statusChanged(tr("Fetching messages from '%1'…").arg(displayName_));

    QPointer<QueueDetailDialog> self = this;

    QFuture<GetMessagesResult> future = QtConcurrent::run(
        [self, streamName, startSeq, count, mode]() -> GetMessagesResult {
            return exception_helper::wrap_async_fetch<GetMessagesResult>(
                [&]() -> GetMessagesResult {
                    if (!self || !self->clientManager_) {
                        return {.success = false, .messages = {}, .mode = mode,
                                .error_message = "Dialog closed", .error_details = {}};
                    }

                    auto admin = self->clientManager_->admin();
                    const std::string stream = streamName.toStdString();

                    std::vector<MessageRow> rows;
                    rows.reserve(static_cast<std::size_t>(count));

                    for (int i = 0; i < count; ++i) {
                        const auto seq = startSeq + static_cast<std::uint64_t>(i);
                        try {
                            auto msg = admin.peek_message(stream, seq);
                            MessageRow r;
                            r.sequence  = msg.sequence;
                            r.subject   = msg.subject;
                            r.timestamp = platform::time::datetime::to_iso8601_utc(msg.timestamp);
                            r.payload   = std::string(
                                reinterpret_cast<const char*>(msg.data.data()),
                                msg.data.size());
                            rows.push_back(std::move(r));

                            if (mode == GetMode::Pop)
                                admin.delete_message(stream, seq);
                        } catch (const std::exception&) {
                            // Sequence may be deleted or past last_seq — stop here
                            break;
                        }
                    }

                    return {.success = true, .messages = std::move(rows),
                            .mode = mode, .error_message = {}, .error_details = {}};
                }, "get messages");
        });

    getWatcher_->setFuture(future);
}

void QueueDetailDialog::onGetMessagesDone() {
    getMessagesAction_->setEnabled(true);

    const auto result = getWatcher_->result();
    if (result.success) {
        populateMessagesTable(result.messages);
        emit statusChanged(tr("Fetched %1 message(s) from '%2'%3")
                           .arg(result.messages.size())
                           .arg(displayName_)
                           .arg(result.mode == GetMode::Pop ? tr(" (popped)") : QString{}));
    } else {
        BOOST_LOG_SEV(lg(), error) << "Get messages failed: "
                                   << result.error_message.toStdString();
        emit errorOccurred(result.error_message);
        MessageBoxHelper::critical(this, tr("Error"),
                                   result.error_message, result.error_details);
    }
}

void QueueDetailDialog::populateMessagesTable(const std::vector<MessageRow>& messages) {
    messagesTable_->setRowCount(static_cast<int>(messages.size()));
    for (int row = 0; row < static_cast<int>(messages.size()); ++row) {
        const auto& m = messages[row];

        auto* seqItem = new QTableWidgetItem(
            QString::number(static_cast<qlonglong>(m.sequence)));
        // Store sequence as user data for delete operations
        seqItem->setData(Qt::UserRole, static_cast<qlonglong>(m.sequence));
        messagesTable_->setItem(row, 0, seqItem);

        messagesTable_->setItem(row, 1,
            new QTableWidgetItem(QString::fromStdString(m.subject)));
        messagesTable_->setItem(row, 2,
            new QTableWidgetItem(QString::fromStdString(m.timestamp)));

        auto* payloadItem = new QTableWidgetItem(
            QString::fromStdString(m.payload));
        payloadItem->setToolTip(QString::fromStdString(m.payload));
        messagesTable_->setItem(row, 3, payloadItem);
    }
    updateDeleteAction();
}

void QueueDetailDialog::onDeleteSelected() {
    const auto selectedRows = messagesTable_->selectedItems();
    if (selectedRows.isEmpty()) return;

    // Collect unique row indices and their sequence numbers
    QSet<int> rowSet;
    for (const auto* item : selectedRows)
        rowSet.insert(item->row());

    std::vector<std::uint64_t> sequences;
    for (int row : rowSet) {
        auto* seqItem = messagesTable_->item(row, 0);
        if (seqItem)
            sequences.push_back(static_cast<std::uint64_t>(
                seqItem->data(Qt::UserRole).toLongLong()));
    }

    if (sequences.empty()) return;

    auto reply = MessageBoxHelper::question(this, tr("Delete Messages"),
        tr("Delete %n message(s) from stream '%1'?", "",
           static_cast<int>(sequences.size())).arg(displayName_));
    if (reply != QMessageBox::Yes) return;

    deleteAction_->setEnabled(false);

    const QString streamName = streamName_;
    QPointer<QueueDetailDialog> self = this;

    struct DeleteResult {
        bool success{false};
        int deleted_count{0};
        QString error_message;
        QString error_details;
    };

    auto* watcher = new QFutureWatcher<DeleteResult>(this);
    connect(watcher, &QFutureWatcher<DeleteResult>::finished,
            this, [self, watcher]() {
        const auto result = watcher->result();
        watcher->deleteLater();
        if (!self) return;

        self->updateDeleteAction();
        if (result.success) {
            // Remove deleted rows from table (reverse order to keep indices valid)
            QList<int> toRemove;
            for (int row = 0; row < self->messagesTable_->rowCount(); ++row) {
                auto* item = self->messagesTable_->item(row, 0);
                if (item && item->isSelected())
                    toRemove.prepend(row);
            }
            for (int row : toRemove)
                self->messagesTable_->removeRow(row);

            emit self->statusChanged(
                tr("Deleted %1 message(s) from '%2'")
                    .arg(result.deleted_count).arg(self->displayName_));
        } else {
            emit self->errorOccurred(result.error_message);
            MessageBoxHelper::critical(self, tr("Delete Error"),
                                       result.error_message, result.error_details);
        }
    });

    watcher->setFuture(QtConcurrent::run(
        [self, streamName, sequences]() -> DeleteResult {
            return exception_helper::wrap_async_fetch<DeleteResult>(
                [&]() -> DeleteResult {
                    if (!self || !self->clientManager_) {
                        return {.success = false, .deleted_count = 0,
                                .error_message = "Dialog closed", .error_details = {}};
                    }

                    auto admin = self->clientManager_->admin();
                    const std::string stream = streamName.toStdString();
                    int count = 0;

                    for (auto seq : sequences) {
                        try {
                            admin.delete_message(stream, seq);
                            ++count;
                        } catch (const std::exception& e) {
                            BOOST_LOG_SEV(lg(), warn)
                                << "Failed to delete seq " << seq
                                << ": " << e.what();
                        }
                    }

                    return {.success = true, .deleted_count = count,
                            .error_message = {}, .error_details = {}};
                }, "delete messages");
        }));
}

void QueueDetailDialog::updateDeleteAction() {
    deleteAction_->setEnabled(
        messagesTable_->selectionModel() &&
        messagesTable_->selectionModel()->hasSelection());
}

}
