/** -*- mode: typescript-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
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
 *
 */

/**
 * The icon vocabulary.
 *
 * Microsoft Fluent UI System Icons, regular variant, recoloured at use. One set,
 * not two, so a concept has one file.
 *
 * The names here are the concepts, and they are the vocabulary: a screen that
 * needs a "history" uses `history`, not a file path. That is what stops two
 * screens using different marks for one idea, which is the failure that costs
 * when there are hundreds of screens. See `doc/entities/icon-reference.md` for
 * the full list and what each is for.
 *
 * Every icon is imported so the bundler includes it. A missing import is a
 * build error rather than an invisible icon.
 */
import add from '../../assets/icons/ic_fluent_add_20_regular.svg';
import apps from '../../assets/icons/ic_fluent_apps_20_regular.svg';
import arrowDownload from '../../assets/icons/ic_fluent_arrow_download_20_regular.svg';
import arrowLeft from '../../assets/icons/ic_fluent_arrow_left_20_regular.svg';
import arrowRotateCounterclockwise from '../../assets/icons/ic_fluent_arrow_rotate_counterclockwise_20_regular.svg';
import arrowSync from '../../assets/icons/ic_fluent_arrow_sync_20_regular.svg';
import arrowTrending from '../../assets/icons/ic_fluent_arrow_trending_20_regular.svg';
import arrowUpload from '../../assets/icons/ic_fluent_arrow_upload_20_regular.svg';
import book from '../../assets/icons/ic_fluent_book_20_regular.svg';
import briefcase from '../../assets/icons/ic_fluent_briefcase_20_regular.svg';
import building from '../../assets/icons/ic_fluent_building_20_regular.svg';
import buildingBank from '../../assets/icons/ic_fluent_building_bank_20_regular.svg';
import buildingSkyscraper from '../../assets/icons/ic_fluent_building_skyscraper_20_regular.svg';
import calendarClock from '../../assets/icons/ic_fluent_calendar_clock_20_regular.svg';
import chartMultiple from '../../assets/icons/ic_fluent_chart_multiple_20_regular.svg';
import checkmark from '../../assets/icons/ic_fluent_checkmark_20_regular.svg';
import checkmarkCircle from '../../assets/icons/ic_fluent_checkmark_circle_20_regular.svg';
import classification from '../../assets/icons/ic_fluent_classification_20_regular.svg';
import clock from '../../assets/icons/ic_fluent_clock_16_regular.svg';
import code from '../../assets/icons/ic_fluent_code_20_regular.svg';
import columnTriple from '../../assets/icons/ic_fluent_column_triple_20_regular.svg';
import contactCard from '../../assets/icons/ic_fluent_contact_card_20_regular.svg';
import copy from '../../assets/icons/ic_fluent_copy_20_regular.svg';
import currencyDollarEuro from '../../assets/icons/ic_fluent_currency_dollar_euro_20_regular.svg';
import database from '../../assets/icons/ic_fluent_database_20_regular.svg';
import deleteIcon from '../../assets/icons/ic_fluent_delete_20_regular.svg';
import deleteDismiss from '../../assets/icons/ic_fluent_delete_dismiss_20_regular.svg';
import desktop from '../../assets/icons/ic_fluent_desktop_20_regular.svg';
import dismiss from '../../assets/icons/ic_fluent_dismiss_20_regular.svg';
import documentTable from '../../assets/icons/ic_fluent_document_table_20_regular.svg';
import edit from '../../assets/icons/ic_fluent_edit_20_regular.svg';
import errorCircle from '../../assets/icons/ic_fluent_error_circle_20_regular.svg';
import filter from '../../assets/icons/ic_fluent_filter_20_regular.svg';
import flag from '../../assets/icons/ic_fluent_flag_20_regular.svg';
import flashFlow from '../../assets/icons/ic_fluent_flash_flow_20_regular.svg';
import folder from '../../assets/icons/ic_fluent_folder_20_regular.svg';
import globe from '../../assets/icons/ic_fluent_globe_20_regular.svg';
import handshake from '../../assets/icons/ic_fluent_handshake_20_regular.svg';
import history from '../../assets/icons/ic_fluent_history_20_regular.svg';
import info from '../../assets/icons/ic_fluent_info_20_regular.svg';
import keyMultiple from '../../assets/icons/ic_fluent_key_multiple_20_regular.svg';
import library from '../../assets/icons/ic_fluent_library_20_regular.svg';
import lockClosed from '../../assets/icons/ic_fluent_lock_closed_20_regular.svg';
import lockOpen from '../../assets/icons/ic_fluent_lock_open_20_regular.svg';
import notepad from '../../assets/icons/ic_fluent_notepad_20_regular.svg';
import organization from '../../assets/icons/ic_fluent_organization_20_regular.svg';
import passwordReset from '../../assets/icons/ic_fluent_password_reset_48_regular.svg';
import peopleTeam from '../../assets/icons/ic_fluent_people_team_20_regular.svg';
import person from '../../assets/icons/ic_fluent_person_20_regular.svg';
import personAccounts from '../../assets/icons/ic_fluent_person_accounts_20_regular.svg';
import personAdd from '../../assets/icons/ic_fluent_person_add_20_regular.svg';
import play from '../../assets/icons/ic_fluent_play_20_regular.svg';
import plugConnected from '../../assets/icons/ic_fluent_plug_connected_20_regular.svg';
import plugConnectedCheckmark from '../../assets/icons/ic_fluent_plug_connected_checkmark_20_regular.svg';
import plugDisconnected from '../../assets/icons/ic_fluent_plug_disconnected_20_regular.svg';
import question from '../../assets/icons/ic_fluent_question_20_regular.svg';
import record from '../../assets/icons/ic_fluent_record_20_regular.svg';
import save from '../../assets/icons/ic_fluent_save_20_regular.svg';
import search from '../../assets/icons/ic_fluent_search_20_regular.svg';
import serverLink from '../../assets/icons/ic_fluent_server_link_20_regular.svg';
import settings from '../../assets/icons/ic_fluent_settings_20_regular.svg';
import star from '../../assets/icons/ic_fluent_star_20_regular.svg';
import table from '../../assets/icons/ic_fluent_table_20_regular.svg';
import tag from '../../assets/icons/ic_fluent_tag_20_regular.svg';
import tasksApp from '../../assets/icons/ic_fluent_tasks_app_20_regular.svg';
import wand from '../../assets/icons/ic_fluent_wand_20_regular.svg';
import warning from '../../assets/icons/ic_fluent_warning_20_regular.svg';
import windowConsole from '../../assets/icons/ic_fluent_window_console_20_regular.svg';

/**
 * Every icon, by concept.
 *
 * This object is the whole vocabulary. Adding an icon means adding a vendor file
 * and an entry here, and then to the reference document, because an icon that is
 * in the code but not the document is one the next person re-adds under a
 * different name.
 */
export const ICONS = {
  add,
  apps,
  arrowDownload,
  arrowLeft,
  arrowRotateCounterclockwise,
  arrowSync,
  arrowTrending,
  arrowUpload,
  book,
  briefcase,
  building,
  buildingBank,
  buildingSkyscraper,
  calendarClock,
  chartMultiple,
  checkmark,
  checkmarkCircle,
  classification,
  clock,
  code,
  columnTriple,
  contactCard,
  copy,
  currencyDollarEuro,
  database,
  delete: deleteIcon,
  deleteDismiss,
  desktop,
  dismiss,
  documentTable,
  edit,
  errorCircle,
  filter,
  flag,
  flashFlow,
  folder,
  globe,
  handshake,
  history,
  info,
  keyMultiple,
  library,
  lockClosed,
  lockOpen,
  notepad,
  organization,
  passwordReset,
  peopleTeam,
  person,
  personAccounts,
  personAdd,
  play,
  plugConnected,
  plugConnectedCheckmark,
  plugDisconnected,
  question,
  record,
  save,
  search,
  serverLink,
  settings,
  star,
  table,
  tag,
  tasksApp,
  wand,
  warning,
  windowConsole,
} as const;

/** The name of an icon in the vocabulary. */
export type IconName = keyof typeof ICONS;

/** Named exports, so a call site reads as the concept it means. */
export {
  add,
  apps,
  arrowDownload,
  arrowLeft,
  arrowRotateCounterclockwise,
  arrowSync,
  arrowTrending,
  arrowUpload,
  book,
  briefcase,
  building,
  buildingBank,
  buildingSkyscraper,
  calendarClock,
  chartMultiple,
  checkmark,
  checkmarkCircle,
  classification,
  clock,
  code,
  columnTriple,
  contactCard,
  copy,
  currencyDollarEuro,
  database,
  deleteDismiss,
  desktop,
  dismiss,
  documentTable,
  edit,
  errorCircle,
  filter,
  flag,
  flashFlow,
  folder,
  globe,
  handshake,
  history,
  info,
  keyMultiple,
  library,
  lockClosed,
  lockOpen,
  notepad,
  organization,
  passwordReset,
  peopleTeam,
  person,
  personAccounts,
  personAdd,
  play,
  plugConnected,
  plugConnectedCheckmark,
  plugDisconnected,
  question,
  record,
  save,
  search,
  serverLink,
  settings,
  star,
  table,
  tag,
  tasksApp,
  wand,
  warning,
  windowConsole,
};
