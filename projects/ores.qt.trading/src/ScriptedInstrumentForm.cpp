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
#include "ores.qt/ScriptedInstrumentForm.hpp"

#include <QPointer>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <boost/uuid/uuid_io.hpp>
#include "ui_ScriptedInstrumentForm.h"
#include "ores.qt/ClientManager.hpp"
#include "ores.trading.api/messaging/instrument_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

ScriptedInstrumentForm::ScriptedInstrumentForm(QWidget* parent)
    : IInstrumentForm(parent),
      ui_(new Ui::ScriptedInstrumentForm) {
    ui_->setupUi(this);
    setupConnections();
}

ScriptedInstrumentForm::~ScriptedInstrumentForm() = default;

void ScriptedInstrumentForm::setupConnections() {
    auto markChanged = [this]() { onFieldChanged(); };
    connect(ui_->tradeTypeCodeEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->scriptNameEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->descriptionEdit, &QPlainTextEdit::textChanged, this, markChanged);
    connect(ui_->scriptBodyEdit, &QPlainTextEdit::textChanged, this, markChanged);
    connect(ui_->eventsJsonEdit, &QPlainTextEdit::textChanged, this, markChanged);
    connect(ui_->underlyingsJsonEdit, &QPlainTextEdit::textChanged, this, markChanged);
    connect(ui_->parametersJsonEdit, &QPlainTextEdit::textChanged, this, markChanged);
}

void ScriptedInstrumentForm::setClientManager(ClientManager* cm) {
    clientManager_ = cm;
}

void ScriptedInstrumentForm::setUsername(const std::string& username) {
    username_ = username;
}

void ScriptedInstrumentForm::clear() {
    instrument_ = trading::domain::scripted_instrument{};
    loaded_ = false;
    dirty_ = false;
    populateFromInstrument();
}

void ScriptedInstrumentForm::setTradeType(const QString& code,
    bool /*has_options*/, bool /*has_extension*/) {
    instrument_.trade_type_code = code.trimmed().toStdString();
}

void ScriptedInstrumentForm::setReadOnly(bool readOnly) {
    ui_->tradeTypeCodeEdit->setReadOnly(readOnly);
    ui_->scriptNameEdit->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->scriptBodyEdit->setReadOnly(readOnly);
    ui_->eventsJsonEdit->setReadOnly(readOnly);
    ui_->underlyingsJsonEdit->setReadOnly(readOnly);
    ui_->parametersJsonEdit->setReadOnly(readOnly);
}

bool ScriptedInstrumentForm::isDirty() const { return dirty_; }
bool ScriptedInstrumentForm::isLoaded() const { return loaded_; }

void ScriptedInstrumentForm::setChangeReason(
    const std::string& code, const std::string& commentary) {
    instrument_.change_reason_code = code;
    instrument_.change_commentary = commentary;
}

void ScriptedInstrumentForm::writeUiToInstrument() {
    instrument_.trade_type_code =
        ui_->tradeTypeCodeEdit->text().trimmed().toStdString();
    instrument_.script_name =
        ui_->scriptNameEdit->text().trimmed().toStdString();
    instrument_.description =
        ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    instrument_.script_body =
        ui_->scriptBodyEdit->toPlainText().toStdString();
    instrument_.events_json =
        ui_->eventsJsonEdit->toPlainText().toStdString();
    instrument_.underlyings_json =
        ui_->underlyingsJsonEdit->toPlainText().toStdString();
    instrument_.parameters_json =
        ui_->parametersJsonEdit->toPlainText().toStdString();
    instrument_.modified_by = username_;
    instrument_.performed_by = username_;
}

void ScriptedInstrumentForm::setInstrument(
    const trading::messaging::instrument_export_result& instrument) {

    const auto* inst =
        std::get_if<trading::domain::scripted_instrument>(&instrument);
    if (!inst) {
        BOOST_LOG_SEV(lg(), warn)
            << "Non-scripted instrument pushed to ScriptedInstrumentForm";
        emit loadFailed(QStringLiteral(
            "Unexpected instrument type for scripted form"));
        return;
    }

    instrument_ = *inst;
    loaded_ = true;
    dirty_ = false;
    populateFromInstrument();
    emitProvenance();
    emit instrumentLoaded();
}

void ScriptedInstrumentForm::populateFromInstrument() {
    const auto block = [this](bool b) {
        ui_->tradeTypeCodeEdit->blockSignals(b);
        ui_->scriptNameEdit->blockSignals(b);
        ui_->descriptionEdit->blockSignals(b);
        ui_->scriptBodyEdit->blockSignals(b);
        ui_->eventsJsonEdit->blockSignals(b);
        ui_->underlyingsJsonEdit->blockSignals(b);
        ui_->parametersJsonEdit->blockSignals(b);
    };

    block(true);
    ui_->tradeTypeCodeEdit->setText(
        QString::fromStdString(instrument_.trade_type_code));
    ui_->scriptNameEdit->setText(
        QString::fromStdString(instrument_.script_name));
    ui_->descriptionEdit->setPlainText(
        QString::fromStdString(instrument_.description));
    ui_->scriptBodyEdit->setPlainText(
        QString::fromStdString(instrument_.script_body));
    ui_->eventsJsonEdit->setPlainText(
        QString::fromStdString(instrument_.events_json));
    ui_->underlyingsJsonEdit->setPlainText(
        QString::fromStdString(instrument_.underlyings_json));
    ui_->parametersJsonEdit->setPlainText(
        QString::fromStdString(instrument_.parameters_json));
    block(false);
}

void ScriptedInstrumentForm::emitProvenance() {
    InstrumentProvenance p;
    p.version = instrument_.version;
    p.modified_by = instrument_.modified_by;
    p.performed_by = instrument_.performed_by;
    p.recorded_at = instrument_.recorded_at;
    p.change_reason_code = instrument_.change_reason_code;
    p.change_commentary = instrument_.change_commentary;
    emit provenanceChanged(p);
}

void ScriptedInstrumentForm::onFieldChanged() {
    if (!loaded_) return;
    dirty_ = true;
    emit changed();
}

void ScriptedInstrumentForm::saveInstrument(
    std::function<void(const std::string&)> on_success,
    std::function<void(const QString&)> on_failure) {

    if (!clientManager_) {
        on_failure(QStringLiteral("Dialog closed"));
        return;
    }

    struct SaveResult { bool success; std::string message; };

    QPointer<ScriptedInstrumentForm> self = this;
    auto* watcher = new QFutureWatcher<SaveResult>(self);
    connect(watcher, &QFutureWatcher<SaveResult>::finished, self,
        [self, watcher,
         on_success = std::move(on_success),
         on_failure = std::move(on_failure)]() {
        auto result = watcher->result();
        watcher->deleteLater();
        if (!self) return;

        if (!result.success) {
            BOOST_LOG_SEV(lg(), error)
                << "Scripted instrument save failed: " << result.message;
            on_failure(QString::fromStdString(result.message));
            return;
        }

        BOOST_LOG_SEV(lg(), info) << "Scripted instrument saved";
        self->dirty_ = false;
        self->emitProvenance();
        on_success(boost::uuids::to_string(self->instrument_.id));
    });

    auto* cm = clientManager_;
    auto instrument = instrument_;
    watcher->setFuture(QtConcurrent::run(
        [cm, instrument = std::move(instrument)]() -> SaveResult {
        if (!cm)
            return {false, "Dialog closed"};
        trading::messaging::save_scripted_instrument_request req;
        req.data = instrument;
        auto r = cm->process_authenticated_request(std::move(req));
        if (!r) return {false, "Failed to communicate with server"};
        return {r->success, r->message};
    }));
}

}
