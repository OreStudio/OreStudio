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
#include "ores.qt/CalendarEventDetailDialog.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/DynamicComboSetup.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/LookupFetcher.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ores.refdata.api/messaging/calendar_event_protocol.hpp"
#include "ui_CalendarEventDetailDialog.h"
#include <QComboBox>
#include <QFutureWatcher>
#include <QMessageBox>
#include <QPlainTextEdit>
#include <QtConcurrent>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

using namespace ores::logging;

CalendarEventDetailDialog::CalendarEventDetailDialog(QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::CalendarEventDetailDialog)
    , clientManager_(nullptr) {

    ui_->setupUi(this);
    WidgetUtils::setupComboBoxes(this);
    setupUi();
    setupCombos();
    setupConnections();
    // Hierarchy tree seam: a future :implements 9B165431-2921-4CAC-A2E8-2C186741E523
    // block is expected to construct a HierarchyModelBuilder-derived model
    // for this entity, wrap it in a HierarchyTreeWidget, and insert that
    // widget into this dialog's layout (e.g. a dedicated tab). Left empty
    // when no entity implements this kind.
    // Composite child-entity tables seam: an :implements
    // 7E4A2C8D-9F1B-4E6A-8D3C-5B2A7E9F1C4D block constructs one QTableWidget
    // + QToolBar per embedded child entity (e.g. identifiers, contact
    // information), wraps each in a tab, and inserts it into this dialog's
    // tab widget. Left empty when no entity implements this kind.
}

CalendarEventDetailDialog::~CalendarEventDetailDialog() {
    delete ui_;
}

QTabWidget* CalendarEventDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* CalendarEventDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* CalendarEventDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

QString CalendarEventDetailDialog::code() const {
    return QString::fromStdString(boost::uuids::to_string(evt_.id));
}

void CalendarEventDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void CalendarEventDetailDialog::setupCombos() {}

void CalendarEventDetailDialog::setupConnections() {
    connect(
        ui_->saveButton, &QPushButton::clicked, this, &CalendarEventDetailDialog::onSaveClicked);
    connect(ui_->deleteButton,
            &QPushButton::clicked,
            this,
            &CalendarEventDetailDialog::onDeleteClicked);
    connect(
        ui_->closeButton, &QPushButton::clicked, this, &CalendarEventDetailDialog::onCloseClicked);

    connect(ui_->idEdit, &QLineEdit::textChanged, this, &CalendarEventDetailDialog::onCodeChanged);
    connect(ui_->calendarCombo,
            &QComboBox::currentIndexChanged,
            this,
            &CalendarEventDetailDialog::onFieldChanged);
    connect(ui_->eventDateEdit,
            &QLineEdit::textChanged,
            this,
            &CalendarEventDetailDialog::onFieldChanged);
    connect(ui_->diaryEntryTypeCombo,
            &QComboBox::currentIndexChanged,
            this,
            &CalendarEventDetailDialog::onFieldChanged);
    connect(
        ui_->nameEdit, &QLineEdit::textChanged, this, &CalendarEventDetailDialog::onFieldChanged);
    connect(ui_->descriptionEdit,
            &QPlainTextEdit::textChanged,
            this,
            &CalendarEventDetailDialog::onFieldChanged);
    connect(
        ui_->sourceEdit, &QLineEdit::textChanged, this, &CalendarEventDetailDialog::onFieldChanged);
}

void CalendarEventDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
    populateCalendarCode();
    populateDiaryEntryType();
}

void CalendarEventDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void CalendarEventDetailDialog::setEvent(const refdata::domain::calendar_event& evt) {
    evt_ = evt;
    updateUiFromEvent();
}

void CalendarEventDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->idEdit->setReadOnly(true);
    ui_->calendarCombo->setEnabled(createMode);
    ui_->eventDateEdit->setReadOnly(!createMode);
    ui_->diaryEntryTypeCombo->setEnabled(createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    if (createMode) {
        evt_.id = boost::uuids::random_generator()();
    }
    hasChanges_ = false;
    updateSaveButtonState();
}

void CalendarEventDetailDialog::markDirty() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void CalendarEventDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->idEdit->setReadOnly(true);
    ui_->calendarCombo->setEnabled(false);
    ui_->eventDateEdit->setReadOnly(true);
    ui_->diaryEntryTypeCombo->setEnabled(false);
    ui_->nameEdit->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->sourceEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void CalendarEventDetailDialog::populateCalendarCode() {
    BOOST_LOG_SEV(lg(), debug) << "Populating calendar_code combo";
    populateDynamicCombo<refdata::domain::calendar>(
        ui_->calendarCombo,
        this,
        clientManager_,
        &fetch_calendars,
        "calendarEventCalendarWatcher",
        [](const auto& t) { return QString::fromStdString(t.name); },
        [](const auto& t) { return QString::fromStdString(t.code); },
        [](const auto& t) { return t.version; },
        [this]() { return QString::fromStdString(evt_.calendar_code); },
        [this](const QString& error) {
            emit errorMessage(tr("Failed to load calendars: %1").arg(error));
        },
        []() {},
        QObject::tr("Loading…"),
        QObject::tr("Failed to load"),
        [](const auto& t) { return QString::fromStdString(t.code); },
        [](const auto&) { return false; },
        QString{});
}
void CalendarEventDetailDialog::populateDiaryEntryType() {
    BOOST_LOG_SEV(lg(), debug) << "Populating diary_entry_type combo";
    populateDynamicCombo<refdata::domain::diary_entry_type>(
        ui_->diaryEntryTypeCombo,
        this,
        clientManager_,
        &fetch_diary_entry_types,
        "calendarEventDiaryWatcher",
        [](const auto& t) { return QString::fromStdString(t.name); },
        [](const auto& t) { return QString::fromStdString(t.code); },
        [](const auto& t) { return t.display_order; },
        [this]() { return QString::fromStdString(evt_.diary_entry_type); },
        [this](const QString& error) {
            emit errorMessage(tr("Failed to load diary entry types: %1").arg(error));
        },
        []() {},
        QObject::tr("Loading…"),
        QObject::tr("Failed to load"),
        [](const auto& t) { return QString::fromStdString(t.code); },
        [](const auto&) { return false; },
        QString{});
}
void CalendarEventDetailDialog::updateUiFromEvent() {
    ui_->idEdit->setText(QString::fromStdString(boost::uuids::to_string(evt_.id)));
    {
        const auto val = QString::fromStdString(evt_.calendar_code);
        const int idx = ui_->calendarCombo->findData(val);
        if (idx >= 0)
            ui_->calendarCombo->setCurrentIndex(idx);
    }
    ui_->eventDateEdit->setText(
        QString::fromStdString(ores::platform::time::datetime::to_iso8601_date(evt_.event_date)));
    {
        const auto val = QString::fromStdString(evt_.diary_entry_type);
        const int idx = ui_->diaryEntryTypeCombo->findData(val);
        if (idx >= 0)
            ui_->diaryEntryTypeCombo->setCurrentIndex(idx);
    }
    ui_->nameEdit->setText(QString::fromStdString(evt_.name));
    ui_->descriptionEdit->setPlainText(
        evt_.description ? QString::fromStdString(*evt_.description) : QString{});
    ui_->sourceEdit->setText(evt_.source ? QString::fromStdString(*evt_.source) : QString{});

    populateProvenance(evt_.version,
                       evt_.modified_by,
                       evt_.performed_by,
                       evt_.recorded_at,
                       evt_.change_reason_code,
                       evt_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void CalendarEventDetailDialog::updateEventFromUi() {
    if (createMode_) {
        evt_.calendar_code = ui_->calendarCombo->currentData().toString().trimmed().toStdString();
    }
    if (createMode_) {
        evt_.event_date = ores::platform::time::datetime::from_iso8601_date(
            ui_->eventDateEdit->text().trimmed().toStdString());
    }
    if (createMode_) {
        evt_.diary_entry_type =
            ui_->diaryEntryTypeCombo->currentData().toString().trimmed().toStdString();
    }
    evt_.name = ui_->nameEdit->text().trimmed().toStdString();
    {
        const auto description_str = ui_->descriptionEdit->toPlainText().trimmed().toStdString();
        evt_.description =
            description_str.empty() ? std::nullopt : std::optional<std::string>(description_str);
    }
    {
        const auto source_str = ui_->sourceEdit->text().trimmed().toStdString();
        evt_.source = source_str.empty() ? std::nullopt : std::optional<std::string>(source_str);
    }
    evt_.modified_by = username_;
}

void CalendarEventDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void CalendarEventDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void CalendarEventDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool CalendarEventDetailDialog::validateInput() {
    const QString id_val = ui_->idEdit->text().trimmed();
    const QString event_date_val = ui_->eventDateEdit->text().trimmed();
    const QString name_val = ui_->nameEdit->text().trimmed();
    const bool calendar_code_selected = ui_->calendarCombo->currentIndex() >= 0;
    const bool diary_entry_type_selected = ui_->diaryEntryTypeCombo->currentIndex() >= 0;
    const QString event_date_date_val = ui_->eventDateEdit->text().trimmed();

    return true && !id_val.isEmpty() && !event_date_val.isEmpty() && !name_val.isEmpty() &&
           calendar_code_selected &&
           diary_entry_type_selected
           // A blank optional date field is valid (nothing to parse); a
           // non-blank one must be a real ISO-8601 date, or the unchecked
           // from_iso8601_date() parse in updateXFromUi() would throw and
           // crash the app on Save.
           &&
           (event_date_date_val.isEmpty() || ores::platform::time::datetime::is_valid_iso8601_date(
                                                 event_date_date_val.toStdString()));
}

void CalendarEventDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot save calendar event while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input", "Please fill in all required fields.");
        return;
    }


    const auto crOpType = createMode_ ? ChangeReasonDialog::OperationType::Create :
                                        ChangeReasonDialog::OperationType::Amend;
    const auto crSel = promptChangeReason(crOpType, hasChanges_, createMode_ ? "system" : "common");
    if (!crSel)
        return;
    evt_.change_reason_code = crSel->reason_code;
    evt_.change_commentary = crSel->commentary;

    updateEventFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving calendar event: " << boost::uuids::to_string(evt_.id);

    QPointer<CalendarEventDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, evt = evt_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::save_calendar_event_request request;
        request.data = evt;
        auto response_result =
            self->clientManager_->process_authenticated_request(std::move(request));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        return {response_result->success, response_result->message};
    };

    auto* watcher = new QFutureWatcher<SaveResult>(self);
    connect(watcher,
            &QFutureWatcher<SaveResult>::finished,
            self,
            [self, watcher, crReasonCode = crSel->reason_code, crCommentary = crSel->commentary]() {
                auto result = watcher->result();
                watcher->deleteLater();

                if (result.success) {
                    BOOST_LOG_SEV(lg(), info) << "Calendar Event saved successfully";
                    QString code = QString::fromStdString(boost::uuids::to_string(self->evt_.id));
                    self->hasChanges_ = false;
                    self->updateSaveButtonState();
                    emit self->evtSaved(code);
                    self->notifySaveSuccess(tr("Calendar Event '%1' saved").arg(code));
                } else {
                    BOOST_LOG_SEV(lg(), error) << "Save failed: " << result.message;
                    QString errorMsg = QString::fromStdString(result.message);
                    emit self->errorMessage(errorMsg);
                    MessageBoxHelper::critical(self, "Save Failed", errorMsg);
                }
            });

    QFuture<SaveResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

void CalendarEventDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot delete calendar event while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(boost::uuids::to_string(evt_.id));
    auto reply = MessageBoxHelper::question(
        this,
        "Delete Calendar Event",
        QString("Are you sure you want to delete calendar event '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel =
        promptChangeReason(ChangeReasonDialog::OperationType::Delete, false, "common");
    if (!crSel)
        return;

    BOOST_LOG_SEV(lg(), info) << "Deleting calendar event: " << boost::uuids::to_string(evt_.id);

    QPointer<CalendarEventDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, id_str = boost::uuids::to_string(evt_.id)]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::delete_calendar_event_request request;
        request.ids = {id_str};
        auto response_result =
            self->clientManager_->process_authenticated_request(std::move(request));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        return {response_result->success, response_result->message};
    };

    auto* watcher = new QFutureWatcher<DeleteResult>(self);
    connect(watcher, &QFutureWatcher<DeleteResult>::finished, self, [self, code, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Calendar Event deleted successfully";
            emit self->statusMessage(QString("Calendar Event '%1' deleted").arg(code));
            emit self->evtDeleted(code);
            self->requestClose();
        } else {
            BOOST_LOG_SEV(lg(), error) << "Delete failed: " << result.message;
            QString errorMsg = QString::fromStdString(result.message);
            emit self->errorMessage(errorMsg);
            MessageBoxHelper::critical(self, "Delete Failed", errorMsg);
        }
    });

    QFuture<DeleteResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}


}
