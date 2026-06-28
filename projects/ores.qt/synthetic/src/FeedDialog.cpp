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
#include "ores.qt/FeedDialog.hpp"
#include "ores.synthetic.api/messaging/market_data_generation_config_protocol.hpp"
#include <QDialogButtonBox>
#include <QFormLayout>
#include <QFutureWatcher>
#include <QMessageBox>
#include <QPointer>
#include <QPushButton>
#include <QVBoxLayout>
#include <QtConcurrent>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <utility>

namespace ores::qt {

using namespace ores::logging;

FeedDialog::FeedDialog(ClientManager* cm, const QString& username, QWidget* parent)
    : QDialog(parent)
    , clientManager_(cm)
    , username_(username)
    , isNew_(true) {

    feed_.id = boost::uuids::random_generator()();
    feed_.party_id = boost::uuids::uuid{};
    feed_.enabled = true;

    BOOST_LOG_SEV(lg(), info) << "Opening feed dialog for a new feed "
                              << boost::uuids::to_string(feed_.id) << ".";
    buildUi();
}

FeedDialog::FeedDialog(ClientManager* cm,
                       const QString& username,
                       const synthetic::domain::market_data_generation_config& existing,
                       QWidget* parent)
    : QDialog(parent)
    , clientManager_(cm)
    , username_(username)
    , isNew_(false)
    , feed_(existing) {

    BOOST_LOG_SEV(lg(), info) << "Opening feed dialog editing feed "
                              << boost::uuids::to_string(feed_.id) << ".";
    buildUi();
}

void FeedDialog::buildUi() {
    setWindowTitle(isNew_ ? tr("New Feed") : tr("Edit Feed"));
    setModal(true);

    auto* layout = new QVBoxLayout(this);
    auto* form = new QFormLayout();

    nameEdit_ = new QLineEdit(this);
    nameEdit_->setText(QString::fromStdString(feed_.name));

    descEdit_ = new QPlainTextEdit(this);
    descEdit_->setPlainText(QString::fromStdString(feed_.description));

    enabledCheck_ = new QCheckBox(tr("Enabled"), this);
    enabledCheck_->setChecked(feed_.enabled);

    form->addRow(tr("Name"), nameEdit_);
    form->addRow(tr("Description"), descEdit_);
    form->addRow(QString(), enabledCheck_);
    layout->addLayout(form);

    auto* buttons = new QDialogButtonBox(QDialogButtonBox::Save | QDialogButtonBox::Cancel, this);
    layout->addWidget(buttons);
    connect(buttons, &QDialogButtonBox::accepted, this, &FeedDialog::onSave);
    connect(buttons, &QDialogButtonBox::rejected, this, &QDialog::reject);
}

void FeedDialog::onSave() {
    auto feed = feed_;
    feed.name = nameEdit_->text().toStdString();
    feed.description = descEdit_->toPlainText().toStdString();
    feed.enabled = enabledCheck_->isChecked();
    feed.modified_by = username_.toStdString();
    feed.change_reason_code = isNew_ ? "system.new_record" : "common.non_material_update";
    feed.change_commentary = "Authored via Market Simulator";
    feed.version = 0;

    const std::string id = boost::uuids::to_string(feed.id);
    BOOST_LOG_SEV(lg(), info) << "Saving feed " << id << " (new=" << isNew_ << ").";

    QPointer<FeedDialog> self = this;
    auto* cm = clientManager_;

    auto task = [cm, feed]() -> std::pair<bool, QString> {
        auto resp = cm->process_authenticated_request(
            synthetic::messaging::save_market_data_generation_config_request::from(feed));
        if (!resp)
            return {false, QString::fromStdString(resp.error())};
        if (!resp->success)
            return {false, QString::fromStdString(resp->message)};
        return {true, {}};
    };

    auto* watcher = new QFutureWatcher<std::pair<bool, QString>>(self);
    connect(watcher, &QFutureWatcher<std::pair<bool, QString>>::finished, self,
            [self, watcher, id]() {
                auto [ok, err] = watcher->result();
                watcher->deleteLater();
                if (!self)
                    return;
                if (!ok) {
                    BOOST_LOG_SEV(lg(), error)
                        << "Save failed for feed " << id << ": " << err.toStdString();
                    QMessageBox::critical(self, self->tr("Save failed"), err);
                    return;
                }
                BOOST_LOG_SEV(lg(), info) << "Saved feed " << id << ".";
                self->accept();
            });
    watcher->setFuture(QtConcurrent::run(task));
}

}
